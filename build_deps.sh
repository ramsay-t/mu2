#!/bin/bash
if [ -d deps ]; then
    DEPS_DIR=deps;
fi

if [ -d ../../deps ]; then
    DEPS_DIR=../../deps
fi

pushd $DEPS_DIR/jsx && ../../rebar compile && popd
pushd $DEPS_DIR/wrangler && ./configure && make && popd
