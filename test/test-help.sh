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
# $ gen-test -T help
#

[[ "$1" =~ ^-u[0-9]+$ ]] &&
u="${1:2}" ||
u=""

diff -u$u -L help.old <(echo \
'$ test -x ./calc
$ ./calc --help
usage: calc [OPTION|INPUT]...
the options are:
  -i|--interact-input  input type: interactive
  -a|--args-input      input type: command line arguments (default)
  -N|--numeral-parser  parser type: numeral
  -E|--expr-parser     parser type: expression (default)
  -s|--free-spaces     let spaces be free separating numeral tokens
     --no-free-spaces  or otherwise do not (default)
  -d|--debug           print some debugging output
  -D|--no-debug        do not print debugging output (default)
     --dump-options    print options and exit
  -V|--verbose         be verbose
  -v|--version         print version numbers and exit
  -?|--help            display this help info and exit'
) -L help.new <(
echo '$ test -x ./calc'
test -x ./calc 2>&1 ||
echo 'command failed: test -x ./calc'

echo '$ ./calc --help'
./calc --help 2>&1 ||
echo 'command failed: ./calc --help'
)

