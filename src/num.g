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
# Grammar for German numerals.
#

#
# Lexical rules:
#

# Whitespaces are treated as given -- they are not ignored. The tokens
# "Million" and "Millionen" are special in the sense that they must be
# bounded by whitespaces.

#
# Grammar rules:
#

numeral
  : "null"
  | "eins"
  | million
  ;

million
  : "eine" "Million" [ million_base ]
  | tausend [ "Millionen" [ million_base ] ]
  ;

million_base
  : "eins"
  | tausend
  ;

tausend
  : "ein" ( ( zehn_base | hundert_base ) [ tausend_base ] | tausend_base )
  | hundert [ tausend_base ]
  | tausend_base
  ;

tausend_base
  : "tausend" [ "eins" | "ein" ( zehn_base | hundert_base ) | hundert ]
  ;

hundert
  : zehn_atom
  | "zwei" [ zehn_base | hundert_base ]
  | "drei" [ "zehn" | "ssig" | zehn_base | hundert_base ]
  | vfan_atom [ "zehn" | "zig" | zehn_base | hundert_base ]
  | ss_long_atom [ zehn_base | hundert_base ]
  | hundert_base
  ;

hundert_base
  : "hundert" [ "eins" | zehn ]
  ;

zehn
  : zehn_atom
  | "ein" zehn_base
  | "zwei" [ zehn_base ]
  | "drei" [ "zehn" | "ssig" | zehn_base ]
  | vfan_atom [ "zehn" | "zig" | zehn_base ]
  | ss_long_atom [ zehn_base ]
  ;

zehn_base
  : "und" ( "zwan" "zig" | "drei" "ssig" | vfan_atom "zig" | ss_root_atom "zig" )
  ;

zehn_atom
  : "zehn"
  | "elf"
  | "zwoelf"
  | "zwan" "zig"
  | ss_root_atom ( "zehn" | "zig" )
  ;

ss_long_atom
  : "sechs"
  | "sieben"
  ;

ss_root_atom
  : "sech"
  | "sieb"
  ;

vfan_atom
  : "vier"
  | "fuenf"
  | "acht"
  | "neun"
  ;

