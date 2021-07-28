#!/bin/bash
cd config
erl -name client@192.168.32.97 -setcookie test -config test -pa ../ebin -s client start_link