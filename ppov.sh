#!/bin/sh
erl -sname ppov@localhost -pa /usr/local/lib/yaws/ebin -s ppov
./kill.sh
