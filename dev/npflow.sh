#!/bin/bash
MYDIR=`dirname $0` && [ ! `echo "$0" | grep '^\/'` ] && MYDIR=`pwd`/$MYDIR

DATADIR=/opt/data

# nmrview Container
VIEW_IMAGE=nmrprocflow/nmrview
VIEW_CONTAINER=nmrview
VIEW_CONF=$MYDIR/nview.conf

# nmrspec Container
SPEC_PORT=8081
SPEC_IMAGE=nmrprocflow/nmrspec
SPEC_CONTAINER=nmrspec
SPEC_CONF=$MYDIR/npflow.conf

CMD=$1

# If you use a named volume, (assumes that your docker version >= 1.9)
# - First you have to create the /opt/data volume
# sudo docker create -v /opt/data --name npflow_data_volume ubuntu
# - Second, uncomment the line below, and
#   comment the line with 'VOLS' specified further with a local directory .
#VOLS="--volumes-from npflow_data_volume"

# If you use a local directory, first you have to create the /opt/data directory
VOLS="-v $DATADIR:/opt/data"

usage() { echo "usage: sh $0 start|stop|ps|restart|logs|update";  exit 1; }

case "$CMD" in
   start)
        # run NMRviewer
        sudo docker run -d --env-file $VIEW_CONF $VOLS --name $VIEW_CONTAINER $VIEW_IMAGE

        # run NMRProcFlow
        sudo docker run -d --env-file $SPEC_CONF $VOLS  -p $SPEC_PORT:80 \
                               --link $VIEW_CONTAINER:nvapp --name $SPEC_CONTAINER $SPEC_IMAGE

        # show Logs
        sudo docker logs $VIEW_CONTAINER
        sudo docker logs $SPEC_CONTAINER
        ;;
   stop)
        sudo docker rm -f $SPEC_CONTAINER $VIEW_CONTAINER
        ;;
   restart)
        ( sh $0 stop; sh $0 start)
        ;;
   logs)
        sudo docker logs $VIEW_CONTAINER
        sudo docker logs $SPEC_CONTAINER
        ;;
   ps)
        sudo docker ps | head -1
        sudo docker ps | grep "nmrprocflow/"
        ;;
   update)
        sudo docker pull $VIEW_IMAGE
        sudo docker pull $SPEC_IMAGE
        ;;
   *) usage
      exit 2
esac

