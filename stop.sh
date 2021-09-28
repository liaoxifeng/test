#!/bin/bash
cd config
erl -noshell -name stop_feng@192.168.31.13 -setcookie test -config test -pa ../ebin -s test stop -extra 'feng@192.168.31.13'