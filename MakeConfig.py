#!/usr/bin/python
import json

dic = {}
dic["ServerPort"] = 5678
dic["ServerIP"] = "localhost"
settings_file = open("config", 'w')
settings_file.write(json.dumps(dic))
settings_file.close()
