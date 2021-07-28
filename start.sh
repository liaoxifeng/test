#!/bin/bash
cd config
erl -name feng@192.168.31.13 -setcookie test -config test -pa ../ebin -s test start -extra zone