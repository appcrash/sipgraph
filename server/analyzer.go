package server

import (
	"bytes"
	"regexp"
	"strings"
	"time"
)

type sessionInfo struct {
	Id              string `json:"session_id"`
	Cmd             string `json:"-"`
	Caller          string `json:"caller"`
	Callee          string `json:"callee"`
	CreateTimestamp int64  `json:"timestamp"`
}

type packetInfo struct {
	Packet          string `json:"packet"`
	CreateTimestamp int64  `json:"timestamp"`
	Seq             int    `json:"seq"`
}

type sessionState struct {
	createInfo      *sessionInfo
	nextSeq         int
	updateTimeStamp int64
}

type DBOperation interface {
	Init(dbDir string) error
	Close()
	StoreNewSession(si *sessionInfo)
	StorePacket(st *sessionState, packet []byte)
	GetSessionById(id string) *sessionInfo
	GetSessionByCaller(caller string, startTs, endTs int64) []*sessionInfo
	GetSessionByCallee(callee string, startTs, endTs int64) []*sessionInfo
	GetAllPacket(sessionId string) []*packetInfo
}

type analyzer struct {
	stateMap map[string]*sessionState
	db       DBOperation
	packetC  chan []byte
}

const (
	maxSessionExpireTimeInSecond = 10 * 60
)

var (
	// Cmd pattern, example: INVITE sip:+123456789@hb.ims.3gppnetwork.org SIP/2.0
	regCmd = regexp.MustCompile(`^([\w]+)\s+.+\s+[sS][iI][pP]/2\.0\s*$`)

	// pattern: Header: Value
	regHeader = regexp.MustCompile(`^([-\w]+)\:\s*(.*)$`)

	// extract Caller/Callee info from  From/To value
	regPhone = regexp.MustCompile(`[^<]*<\s*(?:[tT][eE][lL]|[sS][iI][pP]):\+?(?:86)?(\d+).*`)

	// extract Session-Id line in the first place
	regSession = regexp.MustCompile(`(?i)^Session-Id:\s*(\S+)\s*$`)
)

func NewAnalyzer(db DBOperation) *analyzer {

	a := &analyzer{
		stateMap: make(map[string]*sessionState),
		packetC:  make(chan []byte, 8),
		db:       db,
	}
	go a.loop()
	return a
}

func getNow() int64 {
	return time.Now().UnixNano() / int64(time.Millisecond)
}

func (a *analyzer) loop() {
	ticker := time.NewTicker(10 * time.Second)
	for {
		select {
		case packet := <-a.packetC:
			a.analyzePacket(packet)
		case <-ticker.C:
			a.removeExpiredSession()
		}
	}
}

func (a *analyzer) analyze(packet []byte) {
	a.packetC <- packet
}

func (a *analyzer) analyzePacket(packet []byte) {
	if len(packet) == 0 {
		return
	}
	lines := bytes.Split(packet, []byte{'\r', '\n'})
	if len(lines) <= 4 {
		// not enough info, or bad Packet
		return
	}
	var si sessionInfo
	var counter int
	sessionLine := lines[0]
	if matches := regSession.FindSubmatch(sessionLine); matches == nil {
		logger.Errorf("invalid Packet without Session Id line")
		return
	} else {
		si.Id = string(matches[1])
	}
	now := getNow()
	si.CreateTimestamp = now
	for _, line := range lines[1:] {
		if cmd := regCmd.FindSubmatch(line); cmd != nil {
			si.Cmd = strings.ToLower(string(cmd[1]))
			counter++
		} else if header := regHeader.FindSubmatch(line); header != nil {
			h, v := header[1], header[2]
			switch strings.ToLower(string(h)) {
			case "from":
				cm := regPhone.FindSubmatch(v)
				if cm == nil {
					logger.Errorf("invalid from header value: %v", string(v))
					return
				}
				si.Caller = string(cm[1])
				counter++
			case "to":
				cm := regPhone.FindSubmatch(v)
				if cm == nil {
					logger.Errorf("invalid to header value: %v", string(v))
					return
				}
				si.Callee = string(cm[1])
				counter++
			}
		}

		if counter == 3 {
			// got enough info
			var state *sessionState
			var ok bool
			state, ok = a.stateMap[si.Id]
			if si.Cmd == "invite" && !ok {
				// this is first INVITE other than re-INVITE
				logger.Infof("store new Session: %v", si.Id)
				state = &sessionState{
					createInfo: &si,
					nextSeq:    0,
				}
				a.stateMap[si.Id] = state
				a.db.StoreNewSession(&si)
			}
			if state == nil {
				logger.Errorf("wrong Session state for Session Id:%v", si.Id)
				return
			}
			state.updateTimeStamp = now
			firstLineSize := len(sessionLine) + 2 // extra \r\n
			originPacket := packet[firstLineSize:]
			a.db.StorePacket(state, originPacket)
			state.nextSeq++
			return
		}
	}

	logger.Errorf("invalid Packet without enough info:\n%v\n", string(packet))
}

func (a *analyzer) removeExpiredSession() {
	// all sessions in the map are iterated, poor performance when too many sessions
	var toRemove []string
	now := getNow()
	for sessionId, state := range a.stateMap {
		diffSeconds := (now - state.updateTimeStamp) / 1000
		if diffSeconds > maxSessionExpireTimeInSecond {
			toRemove = append(toRemove, sessionId)
		}
	}
	for _, sessionId := range toRemove {
		logger.Infof("remove expired Session: %v", sessionId)
		delete(a.stateMap, sessionId)
	}

}
