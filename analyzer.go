package sipgraph

import (
	"bytes"
	"regexp"
	"strings"
	"time"
)

type sessionInfo struct {
	id              string
	cmd             string
	caller          string
	callee          string
	createTimestamp int64
}

type packetInfo struct {
	packet          []byte
	createTimestamp int64
	seq             int
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
	// cmd pattern, example: INVITE sip:+123456789@hb.ims.3gppnetwork.org SIP/2.0
	regCmd = regexp.MustCompile(`^([\w]+)\s+.+\s+[sS][iI][pP]/2\.0\s*$`)

	// pattern: Header: Value
	regHeader = regexp.MustCompile(`^([-\w]+)\:\s*(.*)$`)

	// extract caller/callee info from  From/To value
	regPhone = regexp.MustCompile(`[^<]*<\s*(?:[tT][eE][lL]|[sS][iI][pP]):\+?(?:86)?(\d+).*`)

	// extract session-id line in the first place
	regSession = regexp.MustCompile(`(?i)^session-id:\s*(\S+)\s*$`)
)

func NewAnalyzer(dbDir string) *analyzer {
	db := &ldb{}
	if err := db.Init(dbDir); err != nil {
		panic(err)
	}
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
		// not enough info, or bad packet
		return
	}
	var si sessionInfo
	var counter int
	sessionLine := lines[0]
	if matches := regSession.FindSubmatch(sessionLine); matches == nil {
		logger.Errorf("invalid packet without session id line")
		return
	} else {
		si.id = string(matches[1])
	}
	now := getNow()
	si.createTimestamp = now
	for _, line := range lines[1:] {
		if cmd := regCmd.FindSubmatch(line); cmd != nil {
			si.cmd = strings.ToLower(string(cmd[1]))
			counter++
		} else if header := regHeader.FindSubmatch(line); header != nil {
			h, v := header[1], header[2]
			switch strings.ToLower(string(h)) {
			case "from":
				si.caller = string(v)
				counter++
			case "to":
				si.callee = string(v)
				counter++
			}
		}

		if counter == 3 {
			// got enough info
			var state *sessionState
			var ok bool
			state, ok = a.stateMap[si.id]
			if si.cmd == "invite" && !ok {
				// this is first INVITE other than re-INVITE
				logger.Infof("store new session: %v", si.id)
				state = &sessionState{
					createInfo: &si,
					nextSeq:    0,
				}
				a.stateMap[si.id] = state
				a.db.StoreNewSession(&si)
			}
			if state == nil {
				logger.Errorf("wrong session state for session id:%v", si.id)
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

	logger.Errorf("invalid packet without enough info:\n%v\n", packet)
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
		logger.Infof("remove expired session: %v", sessionId)
		delete(a.stateMap, sessionId)
	}

}
