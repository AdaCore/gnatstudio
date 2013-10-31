#!/bin/bash

argv=()
argv[0]="-pwd$PWD"
i=1

for arg in "$@"; do
    case "$arg" in
      -*) argv[$i]="$arg" ;;
      *)  argv[$i]="`echo $(cd $(dirname "$arg"); pwd)/$(basename "$arg")`" ;;
    esac
    i=$((i + 1))
done

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

APP=GPS
if [ -d $DIR/../GPS.app ]; then
    APP=$DIR/../GPS.app
fi
open -a $APP -n -W --args "${argv[@]}"
