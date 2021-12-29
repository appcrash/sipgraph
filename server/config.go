package server

import (
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
	"io/ioutil"
)

type AppConfig struct {
	UdpAddress  string `yaml:"udp_address"`
	HttpAddress string `yaml:"http_address"`
	DatabaseDir string `yaml:"database_dir"`
}

var (
	logger *logrus.Logger
)

func InitConfig(cfgFile string) (cfg AppConfig) {
	data, err := ioutil.ReadFile(cfgFile)
	if err != nil {
		panic("config file not found")
	}
	yaml.Unmarshal(data, &cfg)
	logger = logrus.New()

	return
}
