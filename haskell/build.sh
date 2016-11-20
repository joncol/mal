#!/bin/bash

cd $1
BIN_PATH=$(stack path --local-install-root)/bin
stack build && cp $BIN_PATH/$1* ..
