#!/bin/bash

cd step0
BIN_PATH=$(stack path --local-install-root)/bin
stack build && cp $BIN_PATH/step0-exe ../step0_repl
