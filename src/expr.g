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
# Simple math grammar.
#

#
# Lexical rules:
#

# WS  [ \t]+
# ADD "+" | "plus"
# SUB "-" | "minus"
# MUL "*" | "mul" | "mal"
# DIV "/" | "div"
# POW "^" | "power" | "hoch"
# LIT [0-9]+
# NUM German numeral -- parsed separately

#
# Grammar rules:
#

expr
  : term ( ( ADD | SUB ) term )*
  ;

term
  : factor ( ( MUL | DIV ) factor )*
  ;

factor
  : primary [ POW factor ]
  ;

primary
  : "(" expr ")"
  | ( ADD | SUB ) term
  | LIT
  | NUM
  ;

