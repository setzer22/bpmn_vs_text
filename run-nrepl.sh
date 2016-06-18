#!/bin/bash

export LD_LIBRARY_PATH=$PWD/native/linux64/
mvn clojure:nrepl
