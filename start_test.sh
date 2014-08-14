#!/usr/bin/env sh

DEPS_LIBS=$(echo "$PWD"/deps/* | tr ' ' ':')
ERL_LIBS=$PWD:$DEPS_LIBS erl -conf dev.config -run erTest_start start_stuff
