package server

import (
	"context"
	"net"
)

type UdpServer struct {
	ctx        context.Context
	cancelFunc context.CancelFunc
	addr       *net.UDPAddr
	db         DBOperation
}

const maxUdpPacket = 64 * 1024

func NewUdpServer(addr string, db DBOperation) *UdpServer {
	s := &UdpServer{}
	if udpAddr, err := net.ResolveUDPAddr("udp", addr); err != nil {
		panic("invalid udp address")
	} else {
		s.addr = udpAddr
		s.db = db
	}
	s.ctx, s.cancelFunc = context.WithCancel(context.Background())
	return s
}

func (s *UdpServer) Start() (err error) {
	var conn *net.UDPConn
	if conn, err = net.ListenUDP("udp", s.addr); err != nil {
		return
	}
	ch := make(chan []byte, 10240)

	// receive loop
	go func() {
		logger.Infof("udp starting at %v", s.addr)
		defer logger.Infoln("udp server receive loop done")
		buffer := make([]byte, maxUdpPacket)
		for {
			n, _, e := conn.ReadFromUDP(buffer)
			if e != nil {
				logger.Errorf("read from udp error: %v", e)
				continue
			}
			switch n {
			case maxUdpPacket:
				logger.Errorf("received large and unusual packet, skip it")
				fallthrough
			case 0:
				continue
			}

			bufferCopy := make([]byte, n)
			copy(bufferCopy, buffer[:n])
			select {
			case ch <- bufferCopy:
			default:
			}
			select {
			case <-s.ctx.Done():
				logger.Infof("receive loop done")
				return
			default:
			}
		}

	}()

	// analyze loop
	go func() {
		a := NewAnalyzer(s.db)
		logger.Infoln("analyzing loop starting")
		defer logger.Infoln("analyzing loop done")
		for {
			select {
			case buffer := <-ch:
				a.analyze(buffer)
			case <-s.ctx.Done():
				return
			}
		}
	}()

	return
}

func (s *UdpServer) Stop() {
	s.cancelFunc()
}
