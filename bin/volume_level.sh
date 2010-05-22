#!/bin/sh

state=`amixer get Front | awk 'NR==6 {gsub(/\[|\]/, ""); 1; print $7}'`
vol=`amixer get Master | awk 'NR==6 {gsub(/\[|\]/, ""); 1; print $5}'`

if [ $state == "on" ];
then
    echo $vol
else
    echo "muted"
fi
