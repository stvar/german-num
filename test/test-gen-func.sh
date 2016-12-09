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
# $ gen-test -T gen-func
#

[[ "$1" =~ ^-u[0-9]+$ ]] &&
u="${1:2}" ||
u=""

diff -u$u -L gen-func.old <(echo \
'$ test -x ./calc
$ shopt -s extglob
$ . ../src/commands.sh
$ gen-func() { printf '\''%s\n'\'' "$@"|calc-gen-func; }
$ gen-func a ab
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: a[b]
	if (*n ++ == '\''a'\'') {
		const char* p = n;
		if (*n ++ == '\''b'\'') {
			r = n;
			t = token_t::ab;
			return true;
		}
		else {
			r = p;
			t = token_t::a;
			return true;
		}
	}
	return false;
}
$ gen-func a ab b
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: a[b]|b
	switch (*n ++) {
	case '\''a'\'': {
		const char* p = n;
		if (*n ++ == '\''b'\'') {
			r = n;
			t = token_t::ab;
			return true;
		}
		else {
			r = p;
			t = token_t::a;
			return true;
		}
	}
	case '\''b'\'':
		r = n;
		t = token_t::b;
		return true;
	}
	return false;
}
$ gen-func ab ac
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: a(b|c)
	if (*n ++ == '\''a'\'') {
		switch (*n ++) {
		case '\''b'\'':
			r = n;
			t = token_t::ab;
			return true;
		case '\''c'\'':
			r = n;
			t = token_t::ac;
			return true;
		}
	}
	return false;
}
$ gen-func a ab ac
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: a[b|c]
	if (*n ++ == '\''a'\'') {
		const char* p = n;
		switch (*n ++) {
		case '\''b'\'':
			r = n;
			t = token_t::ab;
			return true;
		case '\''c'\'':
			r = n;
			t = token_t::ac;
			return true;
		default:
			r = p;
			t = token_t::a;
			return true;
		}
	}
	return false;
}
$ gen-func abc abcz abd abdz abe abez
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: ab(c[z]|d[z]|e[z])
	if (*n ++ == '\''a'\'' &&
		*n ++ == '\''b'\'') {
		switch (*n ++) {
		case '\''c'\'': {
			const char* p = n;
			if (*n ++ == '\''z'\'') {
				r = n;
				t = token_t::abcz;
				return true;
			}
			else {
				r = p;
				t = token_t::abc;
				return true;
			}
		}
		case '\''d'\'': {
			const char* p = n;
			if (*n ++ == '\''z'\'') {
				r = n;
				t = token_t::abdz;
				return true;
			}
			else {
				r = p;
				t = token_t::abd;
				return true;
			}
		}
		case '\''e'\'':
			const char* p = n;
			if (*n ++ == '\''z'\'') {
				r = n;
				t = token_t::abez;
				return true;
			}
			else {
				r = p;
				t = token_t::abe;
				return true;
			}
		}
	}
	return false;
}
$ gen-func ab abc abce abd abdz abe abez
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: ab[c[e]|d[z]|e[z]]
	if (*n ++ == '\''a'\'' &&
		*n ++ == '\''b'\'') {
		const char* p = n;
		switch (*n ++) {
		case '\''c'\'': {
			const char* p = n;
			if (*n ++ == '\''e'\'') {
				r = n;
				t = token_t::abce;
				return true;
			}
			else {
				r = p;
				t = token_t::abc;
				return true;
			}
		}
		case '\''d'\'': {
			const char* p = n;
			if (*n ++ == '\''z'\'') {
				r = n;
				t = token_t::abdz;
				return true;
			}
			else {
				r = p;
				t = token_t::abd;
				return true;
			}
		}
		case '\''e'\'': {
			const char* p = n;
			if (*n ++ == '\''z'\'') {
				r = n;
				t = token_t::abez;
				return true;
			}
			else {
				r = p;
				t = token_t::abe;
				return true;
			}
		}
		default:
			r = p;
			t = token_t::ab;
			return true;
		}
	}
	return false;
}
$ gen-func abc abce abcef abcf abd
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: ab(c[e[f]|f]|d)
	if (*n ++ == '\''a'\'' &&
		*n ++ == '\''b'\'') {
		switch (*n ++) {
		case '\''c'\'': {
			const char* p = n;
			switch (*n ++) {
			case '\''e'\'': {
				const char* p = n;
				if (*n ++ == '\''f'\'') {
					r = n;
					t = token_t::abcef;
					return true;
				}
				else {
					r = p;
					t = token_t::abce;
					return true;
				}
			}
			case '\''f'\'':
				r = n;
				t = token_t::abcf;
				return true;
			default:
				r = p;
				t = token_t::abc;
				return true;
			}
		}
		case '\''d'\'':
			r = n;
			t = token_t::abd;
			return true;
		}
	}
	return false;
}
$ gen-func abc abce abcef abcf abcfg abd
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: ab(c[e[f]|f[g]]|d)
	if (*n ++ == '\''a'\'' &&
		*n ++ == '\''b'\'') {
		switch (*n ++) {
		case '\''c'\'': {
			const char* p = n;
			switch (*n ++) {
			case '\''e'\'': {
				const char* p = n;
				if (*n ++ == '\''f'\'') {
					r = n;
					t = token_t::abcef;
					return true;
				}
				else {
					r = p;
					t = token_t::abce;
					return true;
				}
			}
			case '\''f'\'': {
				const char* p = n;
				if (*n ++ == '\''g'\'') {
					r = n;
					t = token_t::abcfg;
					return true;
				}
				else {
					r = p;
					t = token_t::abcf;
					return true;
				}
			}
			default:
				r = p;
				t = token_t::abc;
				return true;
			}
		}
		case '\''d'\'':
			r = n;
			t = token_t::abd;
			return true;
		}
	}
	return false;
}
$ gen-func abc abce abcef abcf abcfg abcfgh abcfgi abd
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: ab(c[e[f]|f[g[h|i]]]|d)
	if (*n ++ == '\''a'\'' &&
		*n ++ == '\''b'\'') {
		switch (*n ++) {
		case '\''c'\'': {
			const char* p = n;
			switch (*n ++) {
			case '\''e'\'': {
				const char* p = n;
				if (*n ++ == '\''f'\'') {
					r = n;
					t = token_t::abcef;
					return true;
				}
				else {
					r = p;
					t = token_t::abce;
					return true;
				}
			}
			case '\''f'\'': {
				const char* p = n;
				if (*n ++ == '\''g'\'') {
					const char* p = n;
					switch (*n ++) {
					case '\''h'\'':
						r = n;
						t = token_t::abcfgh;
						return true;
					case '\''i'\'':
						r = n;
						t = token_t::abcfgi;
						return true;
					default:
						r = p;
						t = token_t::abcfg;
						return true;
					}
				}
				else {
					r = p;
					t = token_t::abcf;
					return true;
				}
			}
			default:
				r = p;
				t = token_t::abc;
				return true;
			}
		}
		case '\''d'\'':
			r = n;
			t = token_t::abd;
			return true;
		}
	}
	return false;
}
$ gen-func abc abce abcef abcf abcfgh abcfgi abd
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: ab(c[e[f]|f[g(h|i)]]|d)
	if (*n ++ == '\''a'\'' &&
		*n ++ == '\''b'\'') {
		switch (*n ++) {
		case '\''c'\'': {
			const char* p = n;
			switch (*n ++) {
			case '\''e'\'': {
				const char* p = n;
				if (*n ++ == '\''f'\'') {
					r = n;
					t = token_t::abcef;
					return true;
				}
				else {
					r = p;
					t = token_t::abce;
					return true;
				}
			}
			case '\''f'\'': {
				const char* p = n;
				if (*n ++ == '\''g'\'') {
					switch (*n ++) {
					case '\''h'\'':
						r = n;
						t = token_t::abcfgh;
						return true;
					case '\''i'\'':
						r = n;
						t = token_t::abcfgi;
						return true;
					}
					return false;
				}
				else {
					r = p;
					t = token_t::abcf;
					return true;
				}
			}
			default:
				r = p;
				t = token_t::abc;
				return true;
			}
		}
		case '\''d'\'':
			r = n;
			t = token_t::abd;
			return true;
		}
	}
	return false;
}
$ gen-func a ab acd ace
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: a[b|c(d|e)]
	if (*n ++ == '\''a'\'') {
		const char* p = n;
		switch (*n ++) {
		case '\''b'\'':
			r = n;
			t = token_t::ab;
			return true;
		case '\''c'\'':
			switch (*n ++) {
			case '\''d'\'':
				r = n;
				t = token_t::acd;
				return true;
			case '\''e'\'':
				r = n;
				t = token_t::ace;
				return true;
			}
			return false;
		default:
			r = p;
			t = token_t::a;
			return true;
		}
	}
	return false;
}
$ gen-func abcd abce ac
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: a(bc(d|e)|c)
	if (*n ++ == '\''a'\'') {
		switch (*n ++) {
		case '\''b'\'':
			if (*n ++ == '\''c'\'') {
				switch (*n ++) {
				case '\''d'\'':
					r = n;
					t = token_t::abcd;
					return true;
				case '\''e'\'':
					r = n;
					t = token_t::abce;
					return true;
				}
			}
			return false;
		case '\''c'\'':
			r = n;
			t = token_t::ac;
			return true;
		}
	}
	return false;
}
$ gen-func abc abd ac
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: a(b(c|d)|c)
	if (*n ++ == '\''a'\'') {
		switch (*n ++) {
		case '\''b'\'':
			switch (*n ++) {
			case '\''c'\'':
				r = n;
				t = token_t::abc;
				return true;
			case '\''d'\'':
				r = n;
				t = token_t::abd;
				return true;
			}
			return false;
		case '\''c'\'':
			r = n;
			t = token_t::ac;
			return true;
		}
	}
	return false;
}
$ gen-func a abc abd ac
bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
	// pattern: a[b(c|d)|c]
	if (*n ++ == '\''a'\'') {
		const char* p = n;
		switch (*n ++) {
		case '\''b'\'':
			switch (*n ++) {
			case '\''c'\'':
				r = n;
				t = token_t::abc;
				return true;
			case '\''d'\'':
				r = n;
				t = token_t::abd;
				return true;
			}
			return false;
		case '\''c'\'':
			r = n;
			t = token_t::ac;
			return true;
		default:
			r = p;
			t = token_t::a;
			return true;
		}
	}
	return false;
}'
) -L gen-func.new <(
echo '$ test -x ./calc'
test -x ./calc 2>&1 ||
echo 'command failed: test -x ./calc'

echo '$ shopt -s extglob'
shopt -s extglob 2>&1 ||
echo 'command failed: shopt -s extglob'

echo '$ . ../src/commands.sh'
. ../src/commands.sh 2>&1 ||
echo 'command failed: . ../src/commands.sh'

echo '$ gen-func() { printf '\''%s\n'\'' "$@"|calc-gen-func; }'
gen-func() { printf '%s\n' "$@"|calc-gen-func; } 2>&1 ||
echo 'command failed: gen-func() { printf '\''%s\n'\'' "$@"|calc-gen-func; }'

echo '$ gen-func a ab'
gen-func a ab 2>&1 ||
echo 'command failed: gen-func a ab'

echo '$ gen-func a ab b'
gen-func a ab b 2>&1 ||
echo 'command failed: gen-func a ab b'

echo '$ gen-func ab ac'
gen-func ab ac 2>&1 ||
echo 'command failed: gen-func ab ac'

echo '$ gen-func a ab ac'
gen-func a ab ac 2>&1 ||
echo 'command failed: gen-func a ab ac'

echo '$ gen-func abc abcz abd abdz abe abez'
gen-func abc abcz abd abdz abe abez 2>&1 ||
echo 'command failed: gen-func abc abcz abd abdz abe abez'

echo '$ gen-func ab abc abce abd abdz abe abez'
gen-func ab abc abce abd abdz abe abez 2>&1 ||
echo 'command failed: gen-func ab abc abce abd abdz abe abez'

echo '$ gen-func abc abce abcef abcf abd'
gen-func abc abce abcef abcf abd 2>&1 ||
echo 'command failed: gen-func abc abce abcef abcf abd'

echo '$ gen-func abc abce abcef abcf abcfg abd'
gen-func abc abce abcef abcf abcfg abd 2>&1 ||
echo 'command failed: gen-func abc abce abcef abcf abcfg abd'

echo '$ gen-func abc abce abcef abcf abcfg abcfgh abcfgi abd'
gen-func abc abce abcef abcf abcfg abcfgh abcfgi abd 2>&1 ||
echo 'command failed: gen-func abc abce abcef abcf abcfg abcfgh abcfgi abd'

echo '$ gen-func abc abce abcef abcf abcfgh abcfgi abd'
gen-func abc abce abcef abcf abcfgh abcfgi abd 2>&1 ||
echo 'command failed: gen-func abc abce abcef abcf abcfgh abcfgi abd'

echo '$ gen-func a ab acd ace'
gen-func a ab acd ace 2>&1 ||
echo 'command failed: gen-func a ab acd ace'

echo '$ gen-func abcd abce ac'
gen-func abcd abce ac 2>&1 ||
echo 'command failed: gen-func abcd abce ac'

echo '$ gen-func abc abd ac'
gen-func abc abd ac 2>&1 ||
echo 'command failed: gen-func abc abd ac'

echo '$ gen-func a abc abd ac'
gen-func a abc abd ac 2>&1 ||
echo 'command failed: gen-func a abc abd ac'
)

