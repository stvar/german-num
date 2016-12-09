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

usage()
{
    echo "usage: $1 [$(sed 's/^://;s/-:$/\x0/;s/[^:]/|-\0/g;s/:/ <arg>/g;s/^|//;s/\x0/-<long>/' <<< "$2")]"
}

quote()
{
    local __n__
    local __v__

    [ -z "$1" -o "$1" == "__n__" -o "$1" == "__v__" ] &&
    return 1

    printf -v __n__ '%q' "$1"
    eval __v__="\"\$$__n__\""
    #!!! echo "!!! 0 __v__='$__v__'"
    test -z "$__v__" && return 0
    printf -v __v__ '%q' "$__v__"
    #!!! echo "!!! 1 __v__='$__v__'"
    printf -v __v__ '%q' "$__v__"  # double quote
    #!!! echo "!!! 2 __v__='$__v__'"
    test -z "$SHELL_BASH_QUOTE_TILDE" &&
    __v__="${__v__//\~/\\~}"
    eval "$__n__=$__v__"
}

assign2()
{
    local __n__
    local __v__

    [ -z "$1" -o "$1" == "__n__" -o "$1" == "__v__" ] && return 1
    [ -z "$2" -o "$2" == "__n__" -o "$2" == "__v__" ] && return 1
    printf -v __n__ '%q' "$2"
    eval __v__="\"\$$__n__\""
    test -n "$__v__" &&
    printf -v __v__ '%q' "$__v__"
    printf -v __n__ '%q' "$1"
    test -z "$SHELL_BASH_QUOTE_TILDE" &&
    __v__="${__v__//\~/\\~}"
    eval "$__n__=$__v__"
}

optopt()
{
    local __n__="${1:-$opt}"       #!!!NONLOCAL
    local __v__=''
    test -n "$__n__" &&
    printf -v __v__ '%q' "$__n__"  # paranoia
    test -z "$SHELL_BASH_QUOTE_TILDE" &&
    __v__="${__v__//\~/\\~}"
    eval "$__n__=$__v__"
}

optarg()
{
    local __n__="${1:-$opt}"       #!!!NONLOCAL
    local __v__=''
    test -n "$OPTARG" &&
    printf -v __v__ '%q' "$OPTARG" #!!!NONLOCAL
    test -z "$SHELL_BASH_QUOTE_TILDE" &&
    __v__="${__v__//\~/\\~}"
    eval "$__n__=$__v__"
}

optact()
{
    local __v__="${1:-$opt}"       #!!!NONLOCAL
    printf -v __v__ '%q' "$__v__"  # paranoia
    test -z "$SHELL_BASH_QUOTE_TILDE" &&
    __v__="${__v__//\~/\\~}"
    eval "act=$__v__"
}

error()
{
    local __self__="$self"     #!!!NONLOCAL
    local __help__="$help"     #!!!NONLOCAL
    local __OPTARG__="$OPTARG" #!!!NONLOCAL
    local __opts__="$opts"     #!!!NONLOCAL
    local __opt__="$opt"       #!!!NONLOCAL
    local __OPT__="$OPT"       #!!!NONLOCAL

    local self="error"

    # actions: \
    #  a:argument for option -$OPTARG not found|
    #  o:when $OPTARG != '?': invalid command line option -$OPTARG, or, \
    #    otherwise, usage|
    #  i:invalid argument '$OPTARG' for option -$opt|
    #  d:option '$OPTARG' does not take arguments|
    #  e:error message|
    #  w:warning message|
    #  u:unexpected option -$opt|
    #  g:when $opt == ':': equivalent with 'a', \
    #    when $opt == '?': equivalent with 'o', \
    #    when $opt is anything else: equivalent with 'u'

    local act="e"
    local A="$__OPTARG__" # $OPTARG
    local h="$__help__"   # $help
    local m=""            # error msg
    local O="$__opts__"   # $opts
    local P="$__opt__"    # $opt
    local L="$__OPT__"    # $OPT
    local S="$__self__"   # $self

    local long=''         # short/long opts (default)

    #!!! echo "!!! A='$A'"
    #!!! echo "!!! O='$O'"
    #!!! echo "!!! P='$P'"
    #!!! echo "!!! L='$L'"
    #!!! echo "!!! S='$S'"

    local opt
    local opts=":aA:degh:iL:m:oO:P:S:uw-:"
    local OPTARG
    local OPTERR=0
    local OPTIND=1
    while getopts "$opts" opt; do
        case "$opt" in
            [adeiouwg])
                act="$opt"
                ;;
            #[])
            #	optopt
            #	;;
            [AhLmOPS])
                optarg
                ;;
            \:)	echo "$self: error: argument for option -$OPTARG not found" >&2
                return 1
                ;;
            \?)	if [ "$OPTARG" != "?" ]; then
                    echo "$self: error: invalid command line option -$OPTARG" >&2
                else
                    echo "$self: $(usage $self $opts)"
                fi
                return 1
                ;;
            -)	case "$OPTARG" in
                    long|long-opts)
                        long='l' ;;
                    short|short-opts)
                        long='' ;;
                    *)	echo "$self: error: invalid command line option --$OPTARG" >&2
                        return 1
                        ;;
                esac
                ;;
            *)	echo "$self: error: unexpected option -$OPTARG" >&2
                return 1
                ;;
        esac
    done
    #!!! echo "!!! A='$A'"
    #!!! echo "!!! O='$O'"
    #!!! echo "!!! P='$P'"
    #!!! echo "!!! L='$L'"
    #!!! echo "!!! S='$S'"
    shift $((OPTIND - 1))
    test -n "$1" && m="$1"
    local f="2"
    if [ "$act" == "g" ]; then
        if [ "$P" == ":" ]; then
            act="a"
        elif [ "$P" == "?" ]; then
            act="o"
        else 
            act="u"
        fi
    fi
    local o=''
    if [ -n "$long" -a -n "$L" ]; then
        test "${L:0:1}" != '-' && o+='--'
        o+="$L"
    elif [[ "$act" == [aod] ]]; then
        o="-$A"
    elif [[ "$act" == [iu] ]]; then
        o="-$P"
    fi
    case "$act" in
        a)	m="argument for option $o not found"
            ;;
        o)	if [ "$A" != "?" ]; then
                m="invalid command line option $o"
            else
                act="h"
                m="$(usage $S $O)"
                f="1"
            fi
            ;;
        i)	m="invalid argument for $o: '$A'"
            ;;
        u)	m="unexpected option $o"
            ;;
        d)	m="option $o does not take arguments"
            ;;
        *)	# [ew]
            if [ "$#" -ge "2" ]; then
                S="$1"
                m="$2"
            elif [ "$#" -ge "1" ]; then
                m="$1"
            fi
            ;;
    esac
    if [ "$act" == "w" ]; then
        m="warning${m:+: $m}"
    elif [ "$act" != "h" ]; then
        m="error${m:+: $m}"
    fi
    if [ -z "$S" -o "$S" == "-" ]; then
        printf "%s\n" "$m" >&$f
    else
        printf "%s: %s\n" "$S" "$m" >&$f
    fi
    if [ "$act" == "h" ]; then
        test -n "$1" && h="$1"
        test -n "$h" &&
        printf "%s\n" "$h" >&$f
    fi
    return $f
}

calc-gen-func()
{
    local self="calc-gen-func"
    local trie="trie"
    local nrex='^(::)?[a-zA-Z_][a-zA-Z0-9_]*(::[a-zA-Z_][a-zA-Z0-9_]*)*$'
    local fdef="Parser::lookup_token"
    local rdef="token_t"
    local tmpf="/tmp/$self.XXX"

    local x="eval"
    local act="C"       # actions: C:gen code (default)|R:gen regex
    local f="+"         # function name (the default is 'lookup')
    local i="-"         # input file (default is stdin)
    local N=""          # do not make 'null' code replacements
    local r="+"         # result type name (the default is 'token_t')
    local v=""          # be verbose

    local opt
    local opts=":Cdf:i:Nr:Rvx"
    local OPTARG
    local OPTERR=0
    local OPTIND=1
    while getopts "$opts" opt; do
        case "$opt" in
            d)	x="echo"
                ;;
            x)	x="eval"
                ;;
            [CR])
                act="$opt"
                ;;
            [Nv])
                optopt
                ;;
            [i])
                optarg
                ;;
            [fr])
                if [ -z "$OPTARG" ]; then
                    error  "argument for -${opt} cannot be null"
                    return 1
                elif [[ "$OPTARG" != "+" && ! ( "$OPTARG" =~ $nrex ) ]]; then
                    error -i
                    return 1;
                fi
                optarg
                ;;
            *)	error -g
                return 1
                ;;
        esac
    done
    shift $((OPTIND - 1))

    local n
    local v2
    for n in f r; do # using $f $r
        assign2 v2 $n
        [ "$v2" == "+" ] &&
        assign2 $n ${n}def # using $fdef $rdef
    done

    local s
    local s2
    local s3
    local sc
    local st
    local sn
    local k
    local c
    if [ "$act" == "R" ]; then
        a='
            {
                # stev: TODO: quote perl regex char of each $i
                s = NF > 1 ? $1 : ""
                for (i = 2; i < NF; i ++)
                    s = length(s) ? s "|" $i : $i
                if (substr($(NF), 1, 1) != "=") {
                    r = $(NF)
                    s = length(s) ? s "|" r : r
                }
                else
                    r = substr($(NF), 2)
                printf("\t\t\ts/^(\\t+)(\\t)return\\s*(\\x22(?:%s)\\x22);\\s*$/'
        [ -n "$v" ] &&
        a+='\\1\\2\\/\\/ input: \\3\\n'
        a+='\\1\\2t = '"$r"'::%s;\\n\\1\\2return true;\\n\\1}/\n", s, r)
            }'
        c="\
awk '$a'"
        $x "$c"
    elif [ "$act" == "C" ]; then
        type -p "$trie" &>/dev/null || {
            error "executable '$trie' not found"
            return 1
        }
        quote trie

        ( # enter a subprocess
        local t="$tmpf"
        # do not quote $tmpf below
        if [ "$x" == "eval" ]; then
            t="$(mktemp $t)" &&
            test -n "$t" || {
                error "inner command failed: mktemp"
                return 1
            }
            quote t
        fi
        trap 'rm -f $t' EXIT

        test "$i" == "-" && i=''
        if [ -n "$i" -a ! -f "$i" ]; then
            error "input file '$i' not found"
            test "$x" == "eval" && return 1
        fi
        quote i
        quote r

        c="\
tee $t${i:+ < $i}|
$self -${v}R -r $r"
        test "$x" == "echo" &&
        echo "$c"
        s2="$(eval "$c")" || {
            error "inner command failed [0]"
            return 1
        }

        a='
            {
                for (i = 1; i < NF; i ++)
                    printf("%s\n", $i)
                if (substr($(NF), 1, 1) != "=")
                    printf("%s\n", $(NF))
            }'
        c="\
awk '$a' \\
$t|
$trie -T ternary -oe -ec -gc"
        test "$x" == "echo" &&
        echo "$c"
        s3="$(eval "$c")" || {
            error "inner command failed [1]"
            return 1
        }

        s='
            1i\
bool '"$f"'(const char* n, '"$r"'::type_t& t)\
{\
\t// pattern: '"$s3"'
            $a}
            :0
            s/^(\t*)    /\1\t/
            t0
            s/(\*\s*)p\b/\1n/g'"${s2:+
$s2}"'
            s/\*n\s*==\s*0\s*\)$/\0 {/
            s/(return\s+)error\b/\1false/
        '
        [ -z "$N" ] && sc='
            H
            $!b
            g
            s/^\n//
            s/^(.*?)(?=\)\n)/\1, const char*\& r/
            s/\s*\&\&\s*\*n\s*==\s*0\s*//Sg
            :0'
        [ -z "$N" ] && {
            st='\t\t\t\t\t\t\t\t\t'
            sn="$((${#st} / 2 - 1))"
        }
        [ -z "$N" ] &&
        for((k=1;k<=sn;k++)); do
            sc+='
            /(?<=\n)('"$st"')if\s*\(\s*\*n\s*==\s*0\s*\)\s*\{\s*?\n('"$st"'[^\n]+\n'"$st"'[^\n]+\n'"$st"'\})\s*?\n('"$st"'if\s*\(\s*\*n\s*\+\+\s*==.*?\)\s*\{\s*?\n.*?\n)('"$st"'\})\s*?(?=\n)/S b1
            /(?<=\n)('"$st"')if\s*\(\s*\*n\s*==\s*0\s*\)\s*\{\s*?\n('"$st"'[^\n]+\n'"$st"'[^\n]+)\n'"$st"'\}\s*?\n('"$st"'switch\s*\(\s*\*n\s*\+\+\s*\)\s*\{\s*?\n.*?)(\n'"$st"'\})\s*?(?=\n)/S b2
            /(?<=\n)('"$st"')if\s*\(\s*\*n\s*==\s*0\s*\)\s*\{\s*?\n\t('"${st:2}"'[^\n]+\n)\t('"${st:2}"'[^\n]+)\n'"$st"'\}\s*?(?=\n)/S b3'
            st="${st:2}"
        done
        [ -z "$N" ] && sc+='
            b4
            :1'
        #!!!RETURN_ELSE
        #!!![ -z "$N" ] && sc+='
        #!!!	s//\1const char* p = n;\n\3\4\n\1else {\n\1\tn = p;\n\2/'
        [ -z "$N" ] && sc+='
            s//\1const char* p = n;\n\3\1\treturn false;\n\4\n\1else {\n\1\tn = p;\n\2/'
        [ -z "$N" ] && sc+='
            b0
            :2'
        [ -z "$N" ] && sc+='
            s//\1const char* p = n;\n\3\n\1\treturn false;\n\1default:\n\1\tn = p;\n\2\4/'
        [ -z "$N" ] && sc+='
            b0
            :3
            s//\2\3/
            b0
            :4
            s/(?<=\n)(\t+)t\s*=\s*token_t\b/\1r = n;\n\0/g
            s/(?<=\n)(\t+)n\s*=\s*p\s*;\s*\n\1r\s*=\s*n\s*(?=;)/\1r = p/g
            :5'
        [ -z "$N" ] && {
            st='\t\t\t\t\t\t\t\t\t'
            sn="$((${#st} / 2))"
        }
        [ -z "$N" ] &&
        for((k=1;k<=sn;k++)); do
            sc+='
            /(?<=\n)('"$st"')(case\b.*?:)\s*(\n\1\tconst\s*char\s*\*\s*p\s*=\s*n\s*;\s*\n.*?\n\1)(?=case|default)/S b6'
            st="${st:2}"
        done
        sc+='
            b7
            :6
            s//\1\2 {\3}\n\1/
            b5
            :7
            s/(return\s+(?:false|true)\s*;\s*\n)\s*return\s+(?:false|true)\s*;\s*\n/\1/Sg
            :8
            /(?<=\n)(\t+)(\treturn\s*true\s*;\s*\n\1\}\s*\n\1else\s*\{\s*\n\1\t[^\n]+\n\1\t[^\n]+\n\1\treturn\s+true\s*;\s*\n\1\})\n\1return\s+false\s*;\s*(?=\n)/ b9
            /(?<=\n)(\t+)(default:\n\1\t[^\n]+\n\1\t[^\n]+\n\1\treturn\s*true\s*;\s*\n\1\}\s*)\n\1return\s+false\s*;\s*(?=\n)/ b9
            /(?<=\n)(\t+)(\tdefault:\n\1\t\t[^\n]+\n\1\t\t[^\n]+\n\1\t\treturn\s*true\s*;\s*\n\1\t\}\s*\n\1\}\n\1else\s*\{\s*\n\1\t[^\n]+\n\1\t[^\n]+\n\1\t[^\n]+\n\1\}\s*)\n\1return\s+false\s*;\s*(?=\n)/ b9
            /(?<=\n)(\t+)(\treturn\s+false\s*;\s*\n\1\}\s*\n\1else\s*\{\s*\n\1\t[^\n]+\n\1\t[^\n]+\n\1\t[^\n]+\n\1\})\s*\n\1return\s+false\s*;\s*(?=\n)/ b9
            b10
            :9
            s//\1\2/
            b8
            :10
            p
            q'
        c="\
awk '$a' \\
$t|
$trie -T ternary -oc -gc|
ssed -R '$s'"
        [ -z "$N" ] && c+="|
ssed -nR '$sc'"

        $x "$c"
        ) # leave the subprocess
    else
        error "internal: unexpected act='$act'"
        return 1
    fi
}

calc-token()
{
    local self="calc-token"
    local tokf="calc.cpp"

    local x="eval"
    local act="T"      # actions: T:token list
    local C=""         # look up tokens defined as 'static consts'
    local g="n"        # grammar type: n:numeral|e:expr

    local opt
    local opts=":Cdg:Tx"
    local OPTARG
    local OPTERR=0
    local OPTIND=1
    while getopts "$opts" opt; do
        case "$opt" in
            d)	x="echo"
                ;;
            x)	x="eval"
                ;;
            [T])
                act="$opt"
                ;;
            [C])
                optopt
                ;;
            [])
                optarg
                ;;
            g)	case "$OPTARG" in
                    n|e)
                        ;;
                    *)	error -i
                        return 1
                        ;;
                esac
                optarg
                ;;
            *)	error -g
                return 1
                ;;
        esac
    done
    shift $((OPTIND - 1))

    local t='^\s*'
    [ -n "$C" ] && t+='static\s+const\s+type_t\s+}'
    t+='([a-z_][a-z0-9_]*)\s*=\s*1ULL\s*<<\s*\d+\s*'
    [ -z "$C" ] && t+=','
    [ -n "$C" ] && t+=';'
    t+='\s*'

    local s
    [ "$g" == "n" ] && s='
        /^\s*namespace\sNum\s*{/!b'
    [ "$g" == "e" ] && s='
        /^\s*namespace\sExpr\s*{/!b'
    s+='
        :0
        n
        /^\s*struct\s+token_t\b/!b0
        :1
        n
        /^\s*{/!b1
        :2
        n
        /^\s*\};/!b3
        g
        /^$/!bq
        :q
        q
        :3
        /'"$t"'\/\/\s*\x27(.*?)\x27.*$/bl
        /'"$t"'\/\/\s*\x22(.*?)\x22.*$/bl
        /'"$t"'.*$/bn
        b2
        :l # literal
        s//\2\t=\1/
        s/^(.*?)\t=\1$/\1/
        bp
        bp
        :n # name
        s//\1/
        :p
        /^\s*(bos|eos)\b/b2
        p
        h
        b2
    '

    "$x" "ssed -nR '$s' $tokf"
}

