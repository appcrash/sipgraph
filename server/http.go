package server

import (
	"encoding/json"
	"fmt"
	"net/http"
	"strconv"
)

type HttpApi struct {
	addr string
	db   DBOperation
}

type sessionData struct {
	Session *sessionInfo  `json:"session"`
	Signal  []*packetInfo `json:"signal"`
}

func NewHttpApi(addr string, db DBOperation) *HttpApi {
	return &HttpApi{addr: addr, db: db}
}

func (h *HttpApi) Serve() {
	http.HandleFunc("/get_session", h.queryBySessionId)
	http.HandleFunc("/get_caller", h.queryByCaller)
	http.HandleFunc("/get_callee", h.queryByCallee)
	http.ListenAndServe(h.addr, nil)
}

func (h *HttpApi) getSessionData(id string) (sd sessionData, err error) {
	si := h.db.GetSessionById(id)
	if si == nil {
		err = fmt.Errorf("Session not exist")
		return
	}
	packets := h.db.GetAllPacket(id)
	sd.Session = si
	sd.Signal = packets
	return
}

func (h *HttpApi) queryBySessionId(w http.ResponseWriter, r *http.Request) {
	if id, ok := r.URL.Query()["id"]; ok && len(id) == 1 {
		sid := id[0]
		var sd sessionData
		var data = []byte("[]")
		var mdata []byte
		var err error
		if sd, err = h.getSessionData(sid); err != nil {
			goto done

		}
		if mdata, err = json.Marshal([]*sessionData{&sd}); err == nil {
			data = mdata
		}
	done:
		w.Header().Set("Content-Type", "application/json,charset=utf-8")
		w.WriteHeader(http.StatusOK)
		w.Write(data)
	} else {
		w.WriteHeader(http.StatusBadRequest)
	}
}

func (h *HttpApi) queryByCaller(w http.ResponseWriter, r *http.Request) {

}

func (h *HttpApi) queryByCallee(w http.ResponseWriter, r *http.Request) {

}

func (h *HttpApi) queryByIndex(w http.ResponseWriter, r *http.Request, field string) {
	var id, startStr, endStr []string
	var tsStart, tsEnd int64
	var si []*sessionInfo
	var ok bool
	var err error
	if id, ok = r.URL.Query()["id"]; !ok || len(id) != 1 {
		goto badRequest
	}
	if startStr, ok = r.URL.Query()["ts_start"]; !ok || len(startStr) != 1 {
		goto badRequest
	} else {
		if tsStart, err = strconv.ParseInt(startStr[0], 10, 64); err != nil {
			goto badRequest
		}
	}
	if endStr, ok = r.URL.Query()["ts_end"]; !ok || len(endStr) != 1 {
		goto badRequest
	} else {
		if tsEnd, err = strconv.ParseInt(endStr[0], 10, 64); err != nil {
			goto badRequest
		}
	}
	switch field {
	case "caller":
		si = h.db.GetSessionByCaller(id[0], tsStart, tsEnd)
	case "callee":
		si = h.db.GetSessionByCallee(id[0], tsStart, tsEnd)
	}

badRequest:
	w.WriteHeader(http.StatusBadRequest)
}
