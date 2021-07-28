#!/bin/bash
if [ ! -d ebin ]; then
	mkdir ebin
fi
cd apps
./complie.sh
cd ..
erl -pa ebin -noshell -s make all -s init stop
