package server

import (
	"encoding/json"
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
	http.HandleFunc("/sip/get_session", h.queryBySessionId)
	http.HandleFunc("/sip/get_caller", h.queryByCaller)
	http.HandleFunc("/sip/get_callee", h.queryByCallee)
	http.ListenAndServe(h.addr, nil)
}

func (h *HttpApi) toSessionData(si *sessionInfo) (sd sessionData) {
	packets := h.db.GetAllPacket(si.Id)
	sd.Session = si
	sd.Signal = packets
	return
}

func (h *HttpApi) queryBySessionId(w http.ResponseWriter, r *http.Request) {
	if id, ok := r.URL.Query()["id"]; ok && len(id) == 1 {
		sid := id[0]
		var sd sessionData
		var data = []byte("[]")
		var jsonData []byte
		var err error

		si := h.db.GetSessionById(sid)
		if si == nil {
			logger.Errorf("session %v not exist", sid)
			goto done
		}
		sd = h.toSessionData(si)
		if jsonData, err = json.Marshal([]*sessionData{&sd}); err == nil {
			data = jsonData
		}
	done:
		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		w.WriteHeader(http.StatusOK)
		w.Write(data)
	} else {
		w.WriteHeader(http.StatusBadRequest)
	}
}

func (h *HttpApi) queryByCaller(w http.ResponseWriter, r *http.Request) {
	h.queryByIndex(w, r, "caller")
}

func (h *HttpApi) queryByCallee(w http.ResponseWriter, r *http.Request) {
	h.queryByIndex(w, r, "callee")
}

func (h *HttpApi) queryByIndex(w http.ResponseWriter, r *http.Request, field string) {
	var id, startStr, endStr []string
	var jsonData []byte
	var tsStart, tsEnd int64
	var sis []*sessionInfo
	var sds []*sessionData
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
		sis = h.db.GetSessionByCaller(id[0], tsStart, tsEnd)
	case "callee":
		sis = h.db.GetSessionByCallee(id[0], tsStart, tsEnd)
	}

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	w.WriteHeader(http.StatusOK)

	if sis != nil {
		for _, si := range sis {
			sd := h.toSessionData(si)
			sds = append(sds, &sd)
		}
		if jsonData, err = json.Marshal(sds); err != nil {
			w.Write([]byte("[]"))
		} else {
			w.Write(jsonData)
		}
		return
	}

	w.Write([]byte("[]"))
	return
badRequest:
	w.WriteHeader(http.StatusBadRequest)
}
