#!/bin/bash

MYDIR=`dirname $0` && [ ! `echo "$0" | grep '^\/'` ] && MYDIR=`pwd`/$MYDIR

DOCKER=`which docker`
DATADIR=/opt/data

DOCKIMG=nmrview
HOST=nvapp
NAME=nvapp
VOLS="-v $DATADIR:/opt/data"
ENV="--env-file ../dev/nvapp.env"

PORT="-p 8000:80"

# docker run -d -v /opt/data:/opt/data -p 8000:80 --name nvapp nmrview
( cd $MYDIR/../nmrviewer
  echo -n "Launch $NAME ($DOCKIMG image) ..."
  [ "x${PORT}x" != "xx" ] && echo -n "$PORT ..."
  $DOCKER run -d $ENV $VOLS $PORT -h $HOST --name $NAME $DOCKIMG 1>/dev/null 2>/dev/null
  $DOCKER ps | grep "$NAME" | grep 'Up ' 1>/dev/null 2>/dev/null
  if [ $? -eq 0 ]; then
     echo "OK"
  else
     echo "Failed"
  fi
)

