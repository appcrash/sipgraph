package main

import (
	"flag"
	"github.com/appcrash/sipgraph/server"
	"io/ioutil"
	"net"
	"time"
)

var (
	configFile string
)

func init() {
	flag.StringVar(&configFile, "c", "config.yml", "path to config file")
}

func main() {
	flag.Parse()
	cfg := server.InitConfig(configFile)
	db := server.InitDB(cfg.DatabaseDir)
	udpServer := server.NewUdpServer(cfg.UdpAddress, db)
	err := udpServer.Start()
	if err != nil {
		panic(err)
	}

	go func() {
		sig, _ := ioutil.ReadFile("/home/yh/develop/sipgraph/invite.dtl")
		<-time.After(1 * time.Second)
		conn, _ := net.Dial("udp", cfg.UdpAddress)
		conn.Write(sig)
	}()
	httpApi := server.NewHttpApi(cfg.HttpAddress, db)
	httpApi.Serve()
}
