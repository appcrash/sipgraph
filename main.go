package main

import (
	"flag"
	"github.com/appcrash/sipgraph/server"
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
	httpApi := server.NewHttpApi(cfg.HttpAddress, db)
	httpApi.Serve()
}
