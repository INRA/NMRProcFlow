#!/bin/bash
MYDIR=`dirname $0` && [ ! `echo "$0" | grep '^\/'` ] && MYDIR=`pwd`/$MYDIR

# If you use a named volume, (assumes that your docker version >= 1.9)
# - First you have to create the /opt/data volume 
# docker create -v /opt/data --name npflow_data_volume ubuntu
# - Second, uncomment the line below, and 
#   comment the line with 'VOLS' specified further with a local directory .
# See https://hostpresto.com/community/tutorials/working-with-docker-volumes/
#     https://docs.docker.com/engine/tutorials/dockervolumes/
VOLS="--volumes-from npflow_data_volume"

# If you use a local directory, first you have to create the /opt/data directory
#VOLS="-v /opt/data:/opt/data"

# run NMRviewer
docker run -d $VOLS  -p 8000:80 --name nmrview nmrprocflow/nmrview

# run NMRProcFlow
docker run -d --env-file $MYDIR/npflow.env $VOLS -p 8081:80 --link nmrview:nvapp --name nmrspec nmrprocflow/nmrspec

docker logs nmrspec
# docker rm -f nmrspec; docker rm -f nmrview


