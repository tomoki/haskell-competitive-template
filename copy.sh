#!/bin/sh


if [ $# -ne 1 ]; then
    echo "usage: copy.sh distdir"
    exit 1
fi
PWD=$1
if [ ! -d $PWD ]; then
    mkdir $PWD
fi
TEMPLATEDIR=`dirname $0`

P1=`realpath $TEMPLATEDIR`
P2=`realpath $PWD`
if [ $P1 = $P2 ] ; then
    echo "DO NOT exec copy.sh in template directory"
    exit 1
fi

cp -r $TEMPLATEDIR/* $PWD
rm -r $PWD/copy.sh

exit 0
