#!/bin/bash

CONF=/var/www/html/nv/etc/nmrview.conf
[ ! -f $CONF ] && echo "ERROR: File $CONF does not exist" && exit 1

DATADIR=`cat $CONF | grep DATASETS | cut -d'=' -f2 | tr -d "\n"`
[ ! -d $DATADIR ] && echo "ERROR: It seems that the DATASETS folder declared in $CONF does not exist" && exit 1
[ -d $DATADIR ] && chmod 777 $DATADIR

for l in `env`; do 
    e=$(echo "$l" | sed -e 's/\//\\&/g; 1{$s/^$/""/}; 1!s/^/"/; $!s/$/"/')
    k=`echo $e | cut -d'=' -f1`
    grep -E "^$k=" $CONF 1>/dev/null 2>/dev/null
    [ $? -eq 0 ] && sed -i -e "s/^$k=.*$/$e/" $CONF
done

set -e

# Apache gets grumpy about PID files pre-existing
rm -f /var/run/apache2/apache2.pid

echo "Starting apache2 server ..."
exec apache2 -DFOREGROUND

