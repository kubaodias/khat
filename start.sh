#!/bin/bash

erl -pa apps/*/ebin -pa deps/*/ebin -config sys -boot start 
