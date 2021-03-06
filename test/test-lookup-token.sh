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
# $ gen-test -T lookup-token
#

[[ "$1" =~ ^-u[0-9]+$ ]] &&
u="${1:2}" ||
u=""

diff -u$u -L lookup-token.old <(echo \
'$ test -x ./calc
$ cd ../src
$ shopt -s extglob
$ . commands.sh
$ print() { printf '\''%s\n'\'' "$@"; }
$ src() { local n="Num"; if [ "$1" == '\''-n'\'' ]; then n="Num"; shift; elif [ "$1" == '\''-e'\'' ]; then n="Expr"; shift; else error "invalid arg '\''$1'\''"; return 1; fi; local a='\'''\''; local p='\''p'\''; [ "$1" == '\''-a'\'' ] && { a='\''='\''; p='\'''\''; }; ssed -nR '\''/^\s*namespace\s+'\''"$n"'\''\s*\{\s*$/!b;:0;n;/^bool\s+Parser::lookup_token\(/!b0;'\''"${a:-p}"'\'';:1;n;'\''"${p:+:2;s/^(\t*)    /\1\t/;t2;$p;}"'\''/^\}/{'\''"${a:+$a;}"'\''q};b1'\'' calc.cpp; }
$ src -n -a
1183
1475
$ src -e -a
2375
2443
$ calc-token -T -gn|calc-gen-func
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: Billi(arde[n]|on[en])|Milli(arde[n]|on[en])|acht|drei|e(in[e|s]|lf)|fuenf|hundert|n(eun|ull)|s(ech[s]|ieb[en]|sig)|tausend|und|vier|z(ehn|ig|w(an|ei|oelf))
	switch (*n ++) {
	case '\''B'\'':
		if (*n ++ == '\''i'\'' &&
			*n ++ == '\''l'\'' &&
			*n ++ == '\''l'\'' &&
			*n ++ == '\''i'\'') {
			switch (*n ++) {
			case '\''a'\'':
				if (*n ++ == '\''r'\'' &&
					*n ++ == '\''d'\'' &&
					*n ++ == '\''e'\'') {
					const char* p = n;
					if (*n ++ == '\''n'\'') {
						r = n;
						t = token_t::billiarden;
						return true;
					}
					else {
						r = p;
						t = token_t::billiarde;
						return true;
					}
				}
				return false;
			case '\''o'\'':
				if (*n ++ == '\''n'\'') {
					const char* p = n;
					if (*n ++ == '\''e'\'' &&
						*n ++ == '\''n'\'') {
						r = n;
						t = token_t::billionen;
						return true;
					}
					else {
						r = p;
						t = token_t::billion;
						return true;
					}
				}
			}
		}
		return false;
	case '\''M'\'':
		if (*n ++ == '\''i'\'' &&
			*n ++ == '\''l'\'' &&
			*n ++ == '\''l'\'' &&
			*n ++ == '\''i'\'') {
			switch (*n ++) {
			case '\''a'\'':
				if (*n ++ == '\''r'\'' &&
					*n ++ == '\''d'\'' &&
					*n ++ == '\''e'\'') {
					const char* p = n;
					if (*n ++ == '\''n'\'') {
						r = n;
						t = token_t::milliarden;
						return true;
					}
					else {
						r = p;
						t = token_t::milliarde;
						return true;
					}
				}
				return false;
			case '\''o'\'':
				if (*n ++ == '\''n'\'') {
					const char* p = n;
					if (*n ++ == '\''e'\'' &&
						*n ++ == '\''n'\'') {
						r = n;
						t = token_t::millionen;
						return true;
					}
					else {
						r = p;
						t = token_t::million;
						return true;
					}
				}
			}
		}
		return false;
	case '\''a'\'':
		if (*n ++ == '\''c'\'' &&
			*n ++ == '\''h'\'' &&
			*n ++ == '\''t'\'') {
			r = n;
			t = token_t::acht;
			return true;
		}
		return false;
	case '\''d'\'':
		if (*n ++ == '\''r'\'' &&
			*n ++ == '\''e'\'' &&
			*n ++ == '\''i'\'') {
			r = n;
			t = token_t::drei;
			return true;
		}
		return false;
	case '\''e'\'':
		switch (*n ++) {
		case '\''i'\'':
			if (*n ++ == '\''n'\'') {
				const char* p = n;
				switch (*n ++) {
				case '\''e'\'':
					r = n;
					t = token_t::eine;
					return true;
				case '\''s'\'':
					r = n;
					t = token_t::eins;
					return true;
				default:
					r = p;
					t = token_t::ein;
					return true;
				}
			}
			return false;
		case '\''l'\'':
			if (*n ++ == '\''f'\'') {
				r = n;
				t = token_t::elf;
				return true;
			}
		}
		return false;
	case '\''f'\'':
		if (*n ++ == '\''u'\'' &&
			*n ++ == '\''e'\'' &&
			*n ++ == '\''n'\'' &&
			*n ++ == '\''f'\'') {
			r = n;
			t = token_t::fuenf;
			return true;
		}
		return false;
	case '\''h'\'':
		if (*n ++ == '\''u'\'' &&
			*n ++ == '\''n'\'' &&
			*n ++ == '\''d'\'' &&
			*n ++ == '\''e'\'' &&
			*n ++ == '\''r'\'' &&
			*n ++ == '\''t'\'') {
			r = n;
			t = token_t::hundert;
			return true;
		}
		return false;
	case '\''n'\'':
		switch (*n ++) {
		case '\''e'\'':
			if (*n ++ == '\''u'\'' &&
				*n ++ == '\''n'\'') {
				r = n;
				t = token_t::neun;
				return true;
			}
			return false;
		case '\''u'\'':
			if (*n ++ == '\''l'\'' &&
				*n ++ == '\''l'\'') {
				r = n;
				t = token_t::null;
				return true;
			}
		}
		return false;
	case '\''s'\'':
		switch (*n ++) {
		case '\''e'\'':
			if (*n ++ == '\''c'\'' &&
				*n ++ == '\''h'\'') {
				const char* p = n;
				if (*n ++ == '\''s'\'') {
					r = n;
					t = token_t::sechs;
					return true;
				}
				else {
					r = p;
					t = token_t::sech;
					return true;
				}
			}
			return false;
		case '\''i'\'':
			if (*n ++ == '\''e'\'' &&
				*n ++ == '\''b'\'') {
				const char* p = n;
				if (*n ++ == '\''e'\'' &&
					*n ++ == '\''n'\'') {
					r = n;
					t = token_t::sieben;
					return true;
				}
				else {
					r = p;
					t = token_t::sieb;
					return true;
				}
			}
			return false;
		case '\''s'\'':
			if (*n ++ == '\''i'\'' &&
				*n ++ == '\''g'\'') {
				r = n;
				t = token_t::ssig;
				return true;
			}
		}
		return false;
	case '\''t'\'':
		if (*n ++ == '\''a'\'' &&
			*n ++ == '\''u'\'' &&
			*n ++ == '\''s'\'' &&
			*n ++ == '\''e'\'' &&
			*n ++ == '\''n'\'' &&
			*n ++ == '\''d'\'') {
			r = n;
			t = token_t::tausend;
			return true;
		}
		return false;
	case '\''u'\'':
		if (*n ++ == '\''n'\'' &&
			*n ++ == '\''d'\'') {
			r = n;
			t = token_t::und;
			return true;
		}
		return false;
	case '\''v'\'':
		if (*n ++ == '\''i'\'' &&
			*n ++ == '\''e'\'' &&
			*n ++ == '\''r'\'') {
			r = n;
			t = token_t::vier;
			return true;
		}
		return false;
	case '\''z'\'':
		switch (*n ++) {
		case '\''e'\'':
			if (*n ++ == '\''h'\'' &&
				*n ++ == '\''n'\'') {
				r = n;
				t = token_t::zehn;
				return true;
			}
			return false;
		case '\''i'\'':
			if (*n ++ == '\''g'\'') {
				r = n;
				t = token_t::zig;
				return true;
			}
			return false;
		case '\''w'\'':
			switch (*n ++) {
			case '\''a'\'':
				if (*n ++ == '\''n'\'') {
					r = n;
					t = token_t::zwan;
					return true;
				}
				return false;
			case '\''e'\'':
				if (*n ++ == '\''i'\'') {
					r = n;
					t = token_t::zwei;
					return true;
				}
				return false;
			case '\''o'\'':
				if (*n ++ == '\''e'\'' &&
					*n ++ == '\''l'\'' &&
					*n ++ == '\''f'\'') {
					r = n;
					t = token_t::zwoelf;
					return true;
				}
			}
		}
	}
	return false;
}
$ print '\''plus =add'\'' '\''minus =sub'\'' mul '\''mal =mul'\'' div '\''power hoch =pow'\''|calc-gen-func
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: div|hoch|m(al|inus|ul)|p(lus|ower)
	switch (*n ++) {
	case '\''d'\'':
		if (*n ++ == '\''i'\'' &&
			*n ++ == '\''v'\'') {
			r = n;
			t = token_t::div;
			return true;
		}
		return false;
	case '\''h'\'':
		if (*n ++ == '\''o'\'' &&
			*n ++ == '\''c'\'' &&
			*n ++ == '\''h'\'') {
			r = n;
			t = token_t::pow;
			return true;
		}
		return false;
	case '\''m'\'':
		switch (*n ++) {
		case '\''a'\'':
			if (*n ++ == '\''l'\'') {
				r = n;
				t = token_t::mul;
				return true;
			}
			return false;
		case '\''i'\'':
			if (*n ++ == '\''n'\'' &&
				*n ++ == '\''u'\'' &&
				*n ++ == '\''s'\'') {
				r = n;
				t = token_t::sub;
				return true;
			}
			return false;
		case '\''u'\'':
			if (*n ++ == '\''l'\'') {
				r = n;
				t = token_t::mul;
				return true;
			}
		}
		return false;
	case '\''p'\'':
		switch (*n ++) {
		case '\''l'\'':
			if (*n ++ == '\''u'\'' &&
				*n ++ == '\''s'\'') {
				r = n;
				t = token_t::add;
				return true;
			}
			return false;
		case '\''o'\'':
			if (*n ++ == '\''w'\'' &&
				*n ++ == '\''e'\'' &&
				*n ++ == '\''r'\'') {
				r = n;
				t = token_t::pow;
				return true;
			}
		}
	}
	return false;
}
$ diff -u0 -Lsrc <(src -n) -Lgen <(calc-token -T -gn|calc-gen-func)
$ diff -u0 -Lsrc <(src -e) -Lgen <(print '\''plus =add'\'' '\''minus =sub'\'' mul '\''mal =mul'\'' div '\''power hoch =pow'\''|calc-gen-func)'
) -L lookup-token.new <(
echo '$ test -x ./calc'
test -x ./calc 2>&1 ||
echo 'command failed: test -x ./calc'

echo '$ cd ../src'
cd ../src 2>&1 ||
echo 'command failed: cd ../src'

echo '$ shopt -s extglob'
shopt -s extglob 2>&1 ||
echo 'command failed: shopt -s extglob'

echo '$ . commands.sh'
. commands.sh 2>&1 ||
echo 'command failed: . commands.sh'

echo '$ print() { printf '\''%s\n'\'' "$@"; }'
print() { printf '%s\n' "$@"; } 2>&1 ||
echo 'command failed: print() { printf '\''%s\n'\'' "$@"; }'

echo '$ src() { local n="Num"; if [ "$1" == '\''-n'\'' ]; then n="Num"; shift; elif [ "$1" == '\''-e'\'' ]; then n="Expr"; shift; else error "invalid arg '\''$1'\''"; return 1; fi; local a='\'''\''; local p='\''p'\''; [ "$1" == '\''-a'\'' ] && { a='\''='\''; p='\'''\''; }; ssed -nR '\''/^\s*namespace\s+'\''"$n"'\''\s*\{\s*$/!b;:0;n;/^bool\s+Parser::lookup_token\(/!b0;'\''"${a:-p}"'\'';:1;n;'\''"${p:+:2;s/^(\t*)    /\1\t/;t2;$p;}"'\''/^\}/{'\''"${a:+$a;}"'\''q};b1'\'' calc.cpp; }'
src() { local n="Num"; if [ "$1" == '-n' ]; then n="Num"; shift; elif [ "$1" == '-e' ]; then n="Expr"; shift; else error "invalid arg '$1'"; return 1; fi; local a=''; local p='p'; [ "$1" == '-a' ] && { a='='; p=''; }; ssed -nR '/^\s*namespace\s+'"$n"'\s*\{\s*$/!b;:0;n;/^bool\s+Parser::lookup_token\(/!b0;'"${a:-p}"';:1;n;'"${p:+:2;s/^(\t*)    /\1\t/;t2;$p;}"'/^\}/{'"${a:+$a;}"'q};b1' calc.cpp; } 2>&1 ||
echo 'command failed: src() { local n="Num"; if [ "$1" == '\''-n'\'' ]; then n="Num"; shift; elif [ "$1" == '\''-e'\'' ]; then n="Expr"; shift; else error "invalid arg '\''$1'\''"; return 1; fi; local a='\'''\''; local p='\''p'\''; [ "$1" == '\''-a'\'' ] && { a='\''='\''; p='\'''\''; }; ssed -nR '\''/^\s*namespace\s+'\''"$n"'\''\s*\{\s*$/!b;:0;n;/^bool\s+Parser::lookup_token\(/!b0;'\''"${a:-p}"'\'';:1;n;'\''"${p:+:2;s/^(\t*)    /\1\t/;t2;$p;}"'\''/^\}/{'\''"${a:+$a;}"'\''q};b1'\'' calc.cpp; }'

echo '$ src -n -a'
src -n -a 2>&1 ||
echo 'command failed: src -n -a'

echo '$ src -e -a'
src -e -a 2>&1 ||
echo 'command failed: src -e -a'

echo '$ calc-token -T -gn|calc-gen-func'
calc-token -T -gn|calc-gen-func 2>&1 ||
echo 'command failed: calc-token -T -gn|calc-gen-func'

echo '$ print '\''plus =add'\'' '\''minus =sub'\'' mul '\''mal =mul'\'' div '\''power hoch =pow'\''|calc-gen-func'
print 'plus =add' 'minus =sub' mul 'mal =mul' div 'power hoch =pow'|calc-gen-func 2>&1 ||
echo 'command failed: print '\''plus =add'\'' '\''minus =sub'\'' mul '\''mal =mul'\'' div '\''power hoch =pow'\''|calc-gen-func'

echo '$ diff -u0 -Lsrc <(src -n) -Lgen <(calc-token -T -gn|calc-gen-func)'
diff -u0 -Lsrc <(src -n) -Lgen <(calc-token -T -gn|calc-gen-func) 2>&1 ||
echo 'command failed: diff -u0 -Lsrc <(src -n) -Lgen <(calc-token -T -gn|calc-gen-func)'

echo '$ diff -u0 -Lsrc <(src -e) -Lgen <(print '\''plus =add'\'' '\''minus =sub'\'' mul '\''mal =mul'\'' div '\''power hoch =pow'\''|calc-gen-func)'
diff -u0 -Lsrc <(src -e) -Lgen <(print 'plus =add' 'minus =sub' mul 'mal =mul' div 'power hoch =pow'|calc-gen-func) 2>&1 ||
echo 'command failed: diff -u0 -Lsrc <(src -e) -Lgen <(print '\''plus =add'\'' '\''minus =sub'\'' mul '\''mal =mul'\'' div '\''power hoch =pow'\''|calc-gen-func)'
)

