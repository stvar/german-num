#!/bin/bash

# Copyright (C) 2016  Stefan Vargyas
# 
# This file is part of German-Num.
# 
# German-Num is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# German-Num is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with German-Num.  If not, see <http://www.gnu.org/licenses/>.

#
# File generated by a command like:
# $ gen-test -w12 -R
#

w=12
c=""
q=""

if [[ "$1" =~ ^-w[1-9][0-9]*$ ]]; then
    w="${1:2}"
    shift
fi
if [ "$1" == "-c" ]; then
    c="c"
    shift
fi
if [ "$1" == "-q" ]; then
    q="q"
fi

p=0
f=0

for t in \
    help \
    gen-func \
    gen-func-ex \
    lookup-token \
    num-set \
    empty \
    space \
    space2 \
    space3 \
    tail-char \
    eins \
    neun \
    calc
do
    test -z "$q" &&
    printf >&2 "%-$((w + 6))s " "test: $t"
    if ! ./test-$t.sh &>/dev/null; then
        (( f ++ ))

        test -n "$q" &&
        echo >&2 -n "test: $t "
        echo >&2 failed
    else
        (( p ++ ))

        test -z "$q" &&
        echo >&2 OK
    fi
done

if [ -z "$c" ]; then
    [ -z "$q" -o "$f" -gt 0 ] &&
    echo >&2
    echo "tests passed: $p"
    echo "tests failed: $f"
else
    echo "$p"
    echo "$f"
fi

exit $(( r != 0 ))

