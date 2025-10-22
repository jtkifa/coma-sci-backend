#!/bin/bash

# send data from .json file via POST to localhost coma-json-server at optional
# PORT with optional WAITTIME

# Called as
#   Send_Post_example.sh hello.json
# or
#   Send_Post_example.sh hello.json 8888 10
# to use port 8888 and wait 10s before downloading result




JSONFILE=$1

if [ "$#" -ge 2 ] ; then
    PORT=$2
else
    PORT=9999
fi

if [ "$#" -eq 3 ] ; then
    WAITTIME=$3
else
    WAITTIME=5
fi

#echo data=$DATA
#echo port=$PORT
#echo SleepTime=$WAITTIME

# extract ID from the JSON file - regexp allows for junk and spaces
# before and after "ID"=
# the 'awk' and 'head' ensure the use of the first ID only

RID=`cat $JSONFILE | grep -i '"id"' | sed -e 's/^.*\"id\" *: *//I' -e 's/,.*$//' -e 's/"//g' | awk '{print $1}' | head -1`


echo "Request ID is $RID"
echo " "

curl -X POST \
     -H "Content-Type: application/json" \
     -d @${JSONFILE}  http://localhost:${PORT}/submit-json

echo " "
echo " "
echo "Sleeping $WAITTIME seconds"
echo " "
sleep $WAITTIME

curl  http://localhost:${PORT}/retrieve-json?id=${RID}

echo " "
echo " "

