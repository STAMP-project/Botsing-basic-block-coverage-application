from sys import argv
import json



content=argv[1]
index=argv[2]
json_string = content.replace("|", ",")
data = json.loads(json_string)
result=data[index].replace('\n', ' ').replace('\r', '')
print result
