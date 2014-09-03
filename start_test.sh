#!/usr/bin/env sh

DEPS_LIBS=$(echo "$PWD"/deps/* | tr ' ' ':')
ERL_LIBS=$PWD:$DEPS_LIBS erl +P 2000000 -config dev.config -run erTest_start start_stuff 
