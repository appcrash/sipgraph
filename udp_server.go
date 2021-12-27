package sipgraph

import (
	"context"
	"net"
)

type UdpServer struct {
	ctx        context.Context
	cancelFunc context.CancelFunc
	addr       *net.UDPAddr
}

func NewUdpServer(addr string) *UdpServer {
	s := &UdpServer{}
	if udpAddr, err := net.ResolveUDPAddr("udp", addr); err != nil {
		panic("invalid udp address")
	} else {
		s.addr = udpAddr
	}
	s.ctx, s.cancelFunc = context.WithCancel(context.Background())

	return s
}

func (s *UdpServer) Start() (err error) {
	var conn *net.UDPConn
	if conn, err = net.ListenUDP("udp", s.addr); err != nil {
		return
	}
	ch := make(chan []byte, 1024)

	// receive loop
	go func() {
		buffer := make([]byte, 4096)
		for {
			_, _, e := conn.ReadFromUDP(buffer)
			if e != nil {
				logger.Errorf("read from udp error: %v", e)
				continue
			}
			select {
			case ch <- buffer:
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
		a := NewAnalyzer()
		for {
			select {
			case buffer := <-ch:
				a.analyze(buffer)
			case <-s.ctx.Done():
				logger.Infof("analyze loop done")
				return
			}
		}
	}()

	return
}

func (s *UdpServer) Stop() {
	s.cancelFunc()
}
