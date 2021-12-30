package server

import (
	"bytes"
	"encoding/gob"
	"fmt"
	"github.com/syndtr/goleveldb/leveldb"
	"github.com/syndtr/goleveldb/leveldb/util"
	"os"
	"regexp"
	"strconv"
)

// ----------------------------------------------------------
// DB            |           KEY           |      VALUE     |
// ==========================================================
// sessionDB     |        session_id       |  session_info  |
// packetDB      | {session_id}_{Seq}_{ts} |  packet bytes  |
// callerIndex   |  {Caller}_{timestamp}   |  session_info  |
// calleeIndex   |  {Callee}_{timestamp}   |  session_info  |

type ldb struct {
	sessionDB *leveldb.DB
	packetDB  *leveldb.DB

	callerIndex *leveldb.DB
	calleeIndex *leveldb.DB
}

var regexPacketKey = regexp.MustCompile(`^([^_]+)_(\d+)_([^_]+)$`)

func InitDB(dbDir string) *ldb {
	db := &ldb{}
	if err := db.Init(dbDir); err != nil {
		panic(err)
	}
	return db
}

func (l *ldb) Init(dbDir string) (err error) {
	if _, e := os.Stat(dbDir); os.IsNotExist(e) {
		os.MkdirAll(dbDir, 0755)
	}
	if l.sessionDB, err = leveldb.OpenFile(dbDir+"Session.db", nil); err != nil {
		return
	}
	if l.packetDB, err = leveldb.OpenFile(dbDir+"Packet.db", nil); err != nil {
		return
	}
	if l.callerIndex, err = leveldb.OpenFile(dbDir+"Caller.index", nil); err != nil {
		return
	}
	l.calleeIndex, err = leveldb.OpenFile(dbDir+"Callee.index", nil)
	return
}

func (l *ldb) Close() {
	for _, db := range []*leveldb.DB{l.sessionDB, l.packetDB, l.calleeIndex, l.calleeIndex} {
		db.Close()
	}
}

func (l *ldb) StoreNewSession(si *sessionInfo) {
	value := l.encodeSession(si)
	if value == nil {
		return
	}
	sessionKey := []byte(si.Id)
	callerKey := []byte(fmt.Sprintf("%s_%d", si.Caller, si.CreateTimestamp))
	calleeKey := []byte(fmt.Sprintf("%s_%d", si.Callee, si.CreateTimestamp))

	l.sessionDB.Put(sessionKey, value, nil)
	l.callerIndex.Put(callerKey, value, nil)
	l.calleeIndex.Put(calleeKey, value, nil)
}

func (l *ldb) StorePacket(st *sessionState, packet []byte) {
	packetKey := []byte(fmt.Sprintf("%s_%d_%d", st.createInfo.Id, st.nextSeq, st.updateTimeStamp))
	l.packetDB.Put(packetKey, packet, nil)
}

func (l *ldb) GetSessionById(id string) *sessionInfo {
	if data, err := l.sessionDB.Get([]byte(id), nil); err != nil {
		return nil
	} else {
		return l.decodeSession(data)
	}
}

func (l *ldb) GetSessionByCaller(caller string, startTs, endTs int64) []*sessionInfo {
	return l.getSessionByIndex(l.callerIndex, caller, startTs, endTs)
}

func (l *ldb) GetSessionByCallee(callee string, startTs, endTs int64) (sis []*sessionInfo) {
	return l.getSessionByIndex(l.calleeIndex, callee, startTs, endTs)
}

func (l *ldb) getSessionByIndex(index *leveldb.DB, id string, startTs, endTs int64) (sis []*sessionInfo) {
	startKey := []byte(fmt.Sprintf("%s_%d", id, startTs))
	endKey := []byte(fmt.Sprintf("%s_%d", id, endTs))
	r := &util.Range{Start: startKey, Limit: endKey}
	i := index.NewIterator(r, nil)
	defer i.Release()
	for i.Next() {
		value := i.Value()
		if si := l.decodeSession(value); si != nil {
			sis = append(sis, si)
		}
	}
	return
}

func (l *ldb) GetAllPacket(sessionId string) (packets []*packetInfo) {
	prefixKey := []byte(fmt.Sprintf("%s_", sessionId))
	i := l.packetDB.NewIterator(util.BytesPrefix(prefixKey), nil)
	defer i.Release()
	var seq int
	var ts int64
	for i.Next() {
		key := i.Key()
		value := i.Value()
		if value != nil {
			if matches := regexPacketKey.FindSubmatch(key); matches == nil {
				logger.Errorf("packet key in db is invalid: %v", key)
				continue
			} else {
				seq, _ = strconv.Atoi(string(matches[2]))
				ts, _ = strconv.ParseInt(string(matches[3]), 10, 64)
				pkt := &packetInfo{
					Packet:          string(value[:]),
					CreateTimestamp: ts,
					Seq:             seq,
				}
				packets = append(packets, pkt)
			}
		}
	}
	return
}

func (l *ldb) decodeSession(data []byte) *sessionInfo {
	var buffer bytes.Buffer
	var si sessionInfo
	buffer.Write(data)
	dec := gob.NewDecoder(&buffer)
	if dec.Decode(&si) != nil {
		return nil
	}
	return &si
}

func (l *ldb) encodeSession(si *sessionInfo) []byte {
	var buffer bytes.Buffer
	enc := gob.NewEncoder(&buffer)
	if enc.Encode(*si) != nil {
		logger.Errorf("failed to encode session into bytes")
		return nil
	}
	return buffer.Bytes()
}
