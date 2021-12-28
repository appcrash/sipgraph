package sipgraph

import (
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
	"io/ioutil"
	"os"
)

type appConfig struct {
	udpAddress  int    `yaml:"udp_address"`
	databaseDir string `yaml:"database_dir"`
}

var logger *logrus.Logger

func initLogger() {
	logger = logrus.New()
}

func main() {
	cfgFile := os.Args[1]
	data, err := ioutil.ReadFile(cfgFile)
	if err != nil {
		panic("config file not found")
	}
	var cfg appConfig
	yaml.Unmarshal(data, &cfg)

	initLogger()
}
