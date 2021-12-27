package sipgraph

import (
	"github.com/sirupsen/logrus"
	"gopkg.in/yaml.v2"
	"io/ioutil"
)

type appConfig struct {
	port int `yaml:"port"`
}

var logger *logrus.Logger

func initLogger() {
	logger = logrus.New()
}

func main() {
	data,err := ioutil.ReadFile("config.yml")
	if err != nil {
		panic("config file not found")
	}
	var cfg appConfig
	yaml.Unmarshal(data,&cfg)

	initLogger()
}


