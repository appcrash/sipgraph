package sipgraph

import (
	"bytes"
	"regexp"
)

type sessionInfo struct {
	seq             int
	activeTimestamp int64
}

type analyzer struct {
	sessionMap map[string]*sessionInfo
}

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

func NewAnalyzer() *analyzer {
	return &analyzer{sessionMap: make(map[string]*sessionInfo)}
}

func (a *analyzer) analyze(packet []byte) {
	if len(packet) == 0 {
		return
	}
	lines := bytes.Split(packet, []byte{'\r', '\n'})
	if len(lines) <= 4 {
		// not enough info, or bad packet
		return
	}
	var sessionId string
	sessionLine := lines[0]
	if matches := regSession.FindSubmatch(sessionLine); matches == nil {
		logger.Errorf("invalid packet without session id line")
		return
	} else {
		sessionId = string(matches[1])
	}

}
