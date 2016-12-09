// Copyright (C) 2016  Stefan Vargyas
// 
// This file is part of German-Num.
// 
// German-Num is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// German-Num is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with German-Num.  If not, see <http://www.gnu.org/licenses/>.

#ifndef __GNUC__
#error we need a GCC compiler
#endif

#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <cstring>
#include <cerrno>
#include <cctype>

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <getopt.h>
#include <unistd.h>

#include <cmath>

#include <stdexcept>
#include <iostream>
#include <iomanip>
#include <sstream>

#include <algorithm>
#include <vector>
#include <limits>

#define STRINGIFY0(S) #S
#define STRINGIFY(S)  STRINGIFY0(S)

const char program[] = STRINGIFY(PROGRAM);
const char verdate[] = "v0.3 2015-08-25"; // $ date +%F

const char license[] =
"Copyright (C) 2016  Stefan Vargyas.\n"
"License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
"This is free software: you are free to change and redistribute it.\n"
"There is NO WARRANTY, to the extent permitted by law.\n";

#define PRINTF_FMT(F) \
    __attribute__ ((format(printf, F, F + 1)))
#define NORETURN \
    __attribute__ ((noreturn))
#define UNUSED \
    __attribute__ ((unused))

#define SYS_UNEXPECT_ERR(M, ...) \
    do { \
        Sys::unexpect_error(__FILE__, __LINE__, M, ## __VA_ARGS__); \
    } \
    while (0)

#ifdef DEBUG
# define SYS_ASSERT(E) \
    do { \
        if (!(E)) \
            Sys::assert_failed(__FILE__, __LINE__, #E); \
    } \
    while (0)
#else
# define SYS_ASSERT(E) \
    do {} while (0)
#endif

namespace Sys {

template<bool b>
struct cxx_assert_t;

template<>
struct cxx_assert_t<true>
{ enum { value }; };

#define CXX_ASSERT(E) \
    do { \
        (void) Sys::cxx_assert_t<(E)>::value; \
    } \
    while (0)

void die(const char *msg, ...)
    PRINTF_FMT(1)
    NORETURN;

void die(const char *msg, ...)
{
    char buf[256];

    va_list args;
    va_start(args, msg);
    vsnprintf(buf, sizeof buf - 1, msg, args);
    va_end(args);
    buf[255] = 0;

    using namespace std;
    cerr << program << ": fatal error: " << buf << endl;

    exit(127);
}

void assert_failed(const char *file, int line, const char *expr)
    NORETURN;

void assert_failed(const char *file, int line, const char *expr)
{
    char buf[256];

    snprintf(buf, sizeof buf - 1, "%s:%d: %s", file, line, expr);
    buf[255] = 0;

    die("assertion failed: %s", buf);
}

void unexpect_error(const char *file, int line, const char *msg, ...)
    PRINTF_FMT(3)
    NORETURN;

void unexpect_error(const char *file, int line, const char *msg, ...)
{
    char buf[256];

    va_list args;
    va_start(args, msg);
    vsnprintf(buf, sizeof buf - 1, msg, args);
    va_end(args);
    buf[255] = 0;

    die("unexpected error:%s:%d: %s", file, line, buf);
}

inline std::string vformat(const char *fmt, va_list args)
{
    char buf[256];

    vsnprintf(buf, sizeof buf - 1, fmt, args);
    buf[255] = 0;

    return buf;
}

inline std::string format(const char *fmt, ...)
    PRINTF_FMT(1);

inline std::string format(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);
    std::string str = vformat(fmt, args);
    va_end(args);

    return str;
}

template<typename T>
void verror(const char* msg, va_list args)
{
    char buf[256];
    vsnprintf(buf, sizeof buf -1, msg, args);
    buf[255] = 0;

    throw T(buf);
}

template<typename T>
void error(const char* msg, ...)
    PRINTF_FMT(1);

template<typename T>
void error(const char* msg, ...)
{
    va_list args;
    va_start(args, msg);
    verror<T>(msg, args);
    //!!!VA_END va_end(args);
}

size_t digits(unsigned val)
{
    if (val == 0)
        return 1;
    size_t r = 0;
    for (; val != 0; val /= 10)
        r ++;
    return r;
}

template<typename T, size_t N>
class array_t
{
public:
    typedef T elem_t;
    typedef T type_t[N];

    array_t(type_t& _a) : a(_a) {}

    elem_t operator[](size_t i) const
    { SYS_ASSERT(i < N); return a[i]; }

private:
    elem_t* a;
};

template<typename T, size_t N, size_t M>
class array_t<T[M], N>
{
public:
    typedef array_t<T, M> elem_t;
    typedef T type_t[N][M];

    array_t(type_t& _a) : a(_a) {}

    elem_t operator[](size_t i) const
    { SYS_ASSERT(i < N); return elem_t(a[i]); }

private:
    typedef T inner_t[M];
    inner_t* a;
};

template<typename T, size_t N>
inline array_t<T, N> array(T (&v)[N])
{ return array_t<T, N>(v); }

template<typename T, size_t N>
inline size_t array_size(T (&v)[N] UNUSED)
{ return N; }

template <typename T>
class Repr;

template <typename T>
inline std::ostream& operator<<(
    std::ostream& ost, const Repr<T>& repr)
{ repr.print(ost); return ost; }

template<typename T>
class ReprPrint
{
public:
    explicit ReprPrint(std::ostream& _ost) :
        ost(_ost)
    {}

    void operator()(const T& val)
    { ost << val; }

private:
    std::ostream& ost;
};

struct repr_type_t
{
    enum char_t {
        quoted_char,
        plain_char
    };

    enum str_t {
        string_str,
        plain_str
    };
};

template<>
class ReprPrint<char>
{
public:
    explicit ReprPrint(std::ostream& _ost) :
        ost(_ost)
    {}

    size_t operator()(char ch, repr_type_t::str_t type)
    {
        const bool str = type == repr_type_t::string_str;

        switch (ch) {
        case '\\':
            return print_out_quoted('\\');
        case '\f':
            return print_out_quoted('f');
        case '\n':
            return print_out_quoted('n');
        case '\r':
            return print_out_quoted('r');
        case '\t':
            return print_out_quoted('t');
        case '\v':
            return print_out_quoted('v');
        case '"':
            if (str)
                return print_out_quoted('"');
            else
                return print_out(ch);
        case '\'':
            if (!str)
                return print_out_quoted('\'');
            else
                return print_out(ch);
        default:
            if (isascii(ch) && !iscntrl(ch))
                return print_out(ch);
            else
                return print_out_hex(ch, str);
        }
    }

    size_t operator()(char ch, repr_type_t::char_t type)
    {
        const bool q = type == repr_type_t::quoted_char;
        size_t n = 0;

        if (q)
            n += print_out('\'');

        n += operator()(ch, q
            ? repr_type_t::plain_str
            : repr_type_t::string_str);

        if (q)
            n += print_out('\'');

        return n;
    }

private:
    size_t print_out(char ch)
    { ost << ch; return 1; }

    size_t print_out_quoted(char ch)
    { ost << '\\' << ch; return 2; }

    size_t print_out_hex(char ch, bool str)
    {
        using namespace std;
        char f = ost.fill();
        streamsize w = ost.width();
        ios_base::fmtflags m = ost.flags();
        unsigned char c = static_cast<unsigned char>(ch);
        ost << "\\x"
            << hex
            << right
            << setfill('0')
            << setw(1 + str)
            << static_cast<unsigned>(c)
            << setfill(f)
            << setw(w);
        ost.flags(m);
        return 3 + (str || c >= '\x10');
    }

    std::ostream& ost;
};

template<>
struct ReprPrint<const char> : public ReprPrint<char>
{
    typedef ReprPrint<char> base_t;

    explicit ReprPrint(std::ostream& _ost) :
        base_t(_ost)
    {}
};

template<typename T>
class ReprPrint<T*>
{
public:
    explicit ReprPrint(std::ostream& _ost) :
        ost(_ost)
    {}

    size_t operator()(T* beg, T* end, bool str)
    {
        if (str)
            ost << '"';
        printer_t r = std::for_each(
            beg, end, printer_t(ost, str));
        if (str)
            ost << '"';
        return str ? r.len + 2 : r.len;
    }

private:
    struct printer_t
    {
        explicit printer_t(std::ostream& _ost, bool _str) :
            repr(_ost), len(0), str(_str)
        {}

        void operator()(char ch)
        { len += repr(ch, str
                    ? repr_type_t::string_str
                    : repr_type_t::plain_str); }

        ReprPrint<T> repr;
        size_t len;
        bool str;
    };

    std::ostream& ost;
};

template<>
class Repr<char>
{
public:
    Repr(char _ch, repr_type_t::char_t _type) :
        ch(_ch), type(_type)
    {}

    void print(std::ostream& ost) const
    { ReprPrint<char> r(ost); r(ch, type); }

private:
    char                ch;
    repr_type_t::char_t type;
};

template<typename T>
class Repr<T*>
{
public:
    Repr(const T* _beg, const T* _end, bool _str) :
        beg(_beg), end(_end), str(_str)
    {}

    Repr(const T* _beg, size_t _size, bool _str) :
        beg(_beg), end(_beg + _size), str(_str)
    {}

    void print(std::ostream& ost) const
    { ReprPrint<const T*> r(ost); r(beg, end, str); }

private:
    const T *beg;
    const T *end;
    bool     str;
};

template<typename T>
inline Repr<T*> repr(T* beg, T* end, bool str = false)
{ return Repr<T*>(beg, end, str); }

template<typename T>
inline Repr<T*> repr(T* ptr, size_t size, bool str = false)
{ return Repr<T*>(ptr, size, str); }

inline Repr<char> repr(char ch, bool quote = false)
{ return Repr<char>(
    ch, quote ? repr_type_t::quoted_char : repr_type_t::plain_char); }

inline Repr<const char*> repr(const char* ptr, bool str = false)
{ return Repr<const char*>(ptr, ptr ? strlen(ptr) : 0, str); }

template<bool, typename>
struct enable_if_t;

template<typename T>
struct enable_if_t<true, T>
{ typedef T type_t; };

template<typename T>
struct num_traits_t :
    private std::numeric_limits<T>
{
    typedef std::numeric_limits<T> base_t;

    using base_t::is_integer;
    using base_t::is_signed;
    using base_t::digits;
    using base_t::min;
    using base_t::max;
};

// stev: partial implementation of integer_cast

template<typename R, typename V>
inline typename enable_if_t<
    num_traits_t<V>::is_integer &&
    num_traits_t<R>::is_integer &&
    !num_traits_t<R>::is_signed &&
    num_traits_t<V>::is_signed &&
    num_traits_t<V>::digits <=
    num_traits_t<R>::digits,
R>::type_t
    integer_cast(V v)
{
    SYS_ASSERT(v >= V(0));
    return static_cast<R>(v);
}

template<typename R, typename V>
inline typename enable_if_t<
    num_traits_t<V>::is_integer &&
    num_traits_t<R>::is_integer &&
    num_traits_t<V>::is_signed &&
    num_traits_t<V>::digits >=
    num_traits_t<R>::digits,
R>::type_t
    integer_cast(V v)
{
    SYS_ASSERT(
        v >= V(num_traits_t<R>::min()) &&
        v <= V(num_traits_t<R>::max()));
    return static_cast<R>(v);
}

template<typename R, typename V>
inline typename enable_if_t<
    num_traits_t<V>::is_integer &&
    num_traits_t<R>::is_integer &&
    !num_traits_t<V>::is_signed &&
    num_traits_t<V>::digits >=
    num_traits_t<R>::digits,
R>::type_t
    integer_cast(V v)
{
    SYS_ASSERT(
        0 <= num_traits_t<R>::max() && 
        v <= V(num_traits_t<R>::max()));
    return static_cast<R>(v);
}

// Determining if an integer is a power of 2
// http://graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2

static inline bool is_pow2(unsigned long long v)
{ return v && !(v & (v - 1)); }

// int __builtin_ctzll(unsigned long long):
// Returns the number of trailing 0-bits in x, starting at the least significant
// bit position. If x is 0, the result is undefined. 
// https://gcc.gnu.org/onlinedocs/gcc-4.3.4/gcc/Other-Builtins.html

static inline size_t log2(unsigned long long v)
{
    SYS_ASSERT(is_pow2(v));
    return integer_cast<size_t>(__builtin_ctzll(v));
}

} // namespace Sys

namespace Base {

class ParserError :
    public std::runtime_error
{
public:
    typedef std::runtime_error base_t;
    typedef std::vector<std::string> msgs_t;

    ParserError(const std::string& _what) :
        base_t(_what),
        msgs(0)
    {}

    ParserError(const std::string& _what, const msgs_t& _msgs) :
        base_t(_what),
        msgs(_msgs)
    {}

    ~ParserError() throw()
    {}

    using base_t::what;

    std::string msg(const char* head) const;

    std::string msg(const std::string& head) const
    { return msg(head.c_str()); }

private:
    msgs_t msgs;
};

struct fmt_t
{
    fmt_t(std::string& _res, const char* _head) :
        res(_res), head(_head)
    {}

    void append(const char* str, bool nl = false) const
    { (!nl ? res : res
        .append("\n"))
        .append(head)
        .append(": parse error:")
        .append(str); };

    void operator()(const std::string& str) const
    { append(str.c_str(), true); }

    std::string& res;
    const char* head;
};

std::string ParserError::msg(const char* head) const
{
    std::string res;
    fmt_t fmt(res, head);

    fmt.append(what());
    for_each(msgs.begin(), msgs.end(), fmt);

    return res;
}

class Parser
{
protected:
    Parser(
#ifdef DEBUG
        bool _debug,
#endif
        const char* const
                   *_token_names,
        size_t      _token_len,
        const char *_input_ptr,
        const char *_input_end,
        size_t      _input_lno = 0,
        bool        _verbose_error = false,
        bool        _free_spaces = false,
        const char *_tail_char = 0);

    virtual ~Parser();

    struct range_t
    {
        range_t() :
            beg(0), end(0)
        {}

        range_t(const char* _beg) :
            beg(_beg), end(_beg)
        {}

        range_t(const char* _beg, const char* _end) :
            beg(_beg), end(_end)
        {}

        size_t size() const { return end - beg; }

        const char *beg;
        const char *end;
    };

    struct token_t
    {
        typedef unsigned long long type_t;

        enum {
            bos = 1ULL << 0,
            eos = 1ULL << 1
        };

        token_t(type_t _type, range_t _lex = range_t()) :
            type(_type),
            lex(_lex)
        {}

        bool operator==(type_t _type) { return type == _type; }

        type_t  type;
        range_t lex;
    };

    class array_t
    {
    public:
        array_t(const char* const* _a, size_t _n) :
            a(_a), n(_n)
        {}

        const char* operator[](size_t i) const
        { SYS_ASSERT(i < n); return a[i]; }

    private:
        const char* const* const a;
        const size_t n;
    };

    virtual void next_token() = 0;

    void error0(size_t pos, const char* msg, va_list args) const;
    void error(size_t pos, const char* msg, ...) const PRINTF_FMT(3);
    void error(const char* msg, ...) const PRINTF_FMT(2);

    const char* token_name(const token_t::type_t type) const
    { return token_names[Sys::log2(type)]; }

    const char* token_name(const token_t& tok) const
    { return token_name(tok.type); }

    bool eat_token(token_t::type_t type)
    { next_token(); return prev_tok.type & type; }

    bool try_token(token_t::type_t type)
    { bool r = tok.type & type; if (r) next_token(); return r; }

    bool peek_token(token_t::type_t type)
    { return tok.type & type; }

    void need_token(token_t::type_t type, bool next = true);
    void missed_token(const char* where);

    size_t pos(const char* ptr) const
    { SYS_ASSERT(ptr >= input_beg); return ptr - input_beg + 1; }

    const char* skip_spaces()
    { const char* p = input_ptr; input_ptr = skip_spaces(p); return p; }

    const char* skip_spaces(const char* ptr) const;

    bool is_tail_char(char ch) const
    { return tail_char && strchr(tail_char, ch); }

    void empty_expr(const char* = 0) const;
    void space_needed(size_t, token_t::type_t, token_t::type_t) const;
    void space_not_allowed(size_t, token_t::type_t, token_t::type_t) const;
    void space_needed(size_t, token_t::type_t) const;
    void unexpected_text(const char*) const;

#ifdef DEBUG
    struct id_t
    {
        explicit id_t(const Parser& _self) :
            self(_self)
        {}
        void print(std::ostream& ost) const
        { self.print_id(ost); }

        friend
        std::ostream& operator<<(std::ostream& ost, const id_t& obj)
        { obj.print(ost); return ost; }

        const Parser& self;
    };
    id_t id() const
    { return id_t(*this); }

    virtual void print_id(std::ostream&) const = 0;

    enum dump_mark_t {
        none_mark,
        enter_mark,
        leave_mark
    };
    void dump(const char* where, dump_mark_t mark = none_mark) const;
#endif

#ifdef DEBUG
    bool        debug;
#endif
    array_t     token_names;

    const char *input_beg;
    const char *input_ptr;
    const char *input_end;
    size_t      input_lno;

    unsigned    verbose_error : 1;
    unsigned    free_spaces : 1;

    const char *tail_char;

    token_t     prev_tok;
    token_t     tok;

private:
    Parser(const Parser&);
    Parser& operator==(const Parser&);
};

Parser::Parser(
#ifdef DEBUG
    bool _debug,
#endif
    const char* const
               *_token_names,
    size_t      _token_len,
    const char *_input_ptr,
    const char *_input_end,
    size_t      _input_lno,
    bool        _verbose_error,
    bool        _free_spaces,
    const char *_tail_char)
:
#ifdef DEBUG
    debug(_debug),
#endif
    token_names(_token_names, _token_len),
    input_beg(_input_ptr),
    input_ptr(_input_ptr),
    input_end(_input_end),
    input_lno(_input_lno),
    verbose_error(_verbose_error),
    free_spaces(_free_spaces),
    tail_char(_tail_char),
    prev_tok(
        token_t::eos,
        range_t(_input_ptr)),
    tok(token_t::bos,
        range_t(_input_ptr))
{}

Parser::~Parser()
{}

static std::string quote_non_plain(
    const char* beg, const char* end, const char* ptr,
    size_t& len)
{
    using namespace std;
    using namespace Sys;

    SYS_ASSERT(
        ptr >= beg &&
        ptr <= end);

    len = 0;
    stringstream ost;
    ReprPrint<char> repr(ost);
    for (const char* p = beg; p < end; p ++) {
        size_t r = repr(*p, repr_type_t::plain_char);
        SYS_ASSERT(r > 0);
        if (p < ptr && r > 1)
            len += r - 1;
    }

    return ost.str();
}

static inline std::string format_err(
    std::pair<size_t, size_t> pos, const std::string& str)
{ return Sys::format("%zu:%zu: %s", pos.first, pos.second, str.c_str()); }

void Parser::error0(size_t pos, const char* msg, va_list args) const
{
    using namespace std;

    const pair<size_t, size_t> err =
        make_pair(input_lno, pos);
    const string what =
        format_err(err, Sys::vformat(msg, args));

    if (verbose_error) {
        SYS_ASSERT(pos > 0);

        const char* ptr = input_beg + (pos - 1);
        SYS_ASSERT(ptr <= input_end);

        size_t len = 0;
        string str = quote_non_plain(
            input_beg, input_end, ptr, len);

        ParserError::msgs_t msgs;
        msgs.push_back(
            format_err(err, str));
        msgs.push_back(
            format_err(err,
                string(pos - 1 + len, ' ').append("^")));

        throw ParserError(what, msgs);
    }
    else
        throw ParserError(what);
}

void Parser::error(const char* msg, ...) const
{
    va_list args;
    va_start(args, msg);
    error0(pos(input_ptr), msg, args);
    //!!!VA_END va_end(args);
}

void Parser::error(size_t pos, const char* msg, ...) const
{
    va_list args;
    va_start(args, msg);
    error0(pos, msg, args);
    //!!!VA_END va_end(args);
}

void Parser::need_token(token_t::type_t type, bool next)
{
    if (tok.type != type)
        error(
            pos(tok.lex.beg),
            "expected token %s but got %s",
            token_name(type), token_name(tok));
    if (next)
        next_token();
}

void Parser::missed_token(const char* where)
{
    const char *ptr = strchr(where, '_');
    error(
        pos(tok.lex.beg),
        "unexpected token %s in %s",
        token_name(tok), ptr ? ptr + 1 : where);
}

const char* Parser::skip_spaces(const char* ptr) const
{
    using namespace std;
    const char *ptr2 =
        find_if(ptr, input_end, not1(ptr_fun(::isblank)));
#ifdef DEBUG
    if (debug && ptr2 > ptr) {
        cout
            << left << setw(2) << id()
            << left << setw(19) << __func__
            << right << setw(3) << pos(ptr)
            << right << setw(6) << ' '
            << right << setw(3) << pos(ptr2)
            << right << setw(37) << " ^" << Sys::repr(ptr, ptr2) << '$'
            << endl;
    }
#endif
    return ptr2;
}

void Parser::empty_expr(const char* name) const
{ 
    error("empty %-*sexpression",
        Sys::integer_cast<int>(
            name ? 1 + strlen(name) : 0),
        name ? name : "");
}

void Parser::space_needed(
    size_t pos, token_t::type_t tok1, token_t::type_t tok2) const
{
    error(pos, "space needed between %s and %s tokens",
        token_name(tok1), token_name(tok2));
}

void Parser::space_not_allowed(
    size_t pos, token_t::type_t tok1, token_t::type_t tok2) const
{
    error(pos, "space not allowed between %s and %s tokens",
        token_name(tok1), token_name(tok2));
}

void Parser::space_needed(size_t pos, token_t::type_t tok) const
{
    error(pos, "space needed after %s token",
        token_name(tok));
}

void Parser::unexpected_text(const char* beg) const
{
    std::ostringstream ost;
    ost << Sys::repr(beg, input_end);

    error(pos(beg), "unexpected text '%s'", ost.str().c_str());
}

#ifdef DEBUG

void Parser::dump(const char* where, dump_mark_t mark) const
{
    static const char marks[] = {
        ' ', // dump_mark_t::none_mark
        '>', // dump_mark_t::enter_mark
        '<', // dump_mark_t::leave_mark
    };

    using namespace std;
    const char *end = input_ptr + 32;
    const char *name = token_name(tok);
    size_t cpos = pos(input_ptr);
    size_t tpos = pos(tok.lex.beg);
    if (end > input_end)
        end = input_end;
    cout
        << left << setw(2) << id()
        << left << setw(18) << where
        << Sys::array(marks)[mark]
        << right << setw(3) << cpos
        << " tok: "
        << right << setw(3) << tpos
        << " type=" << name
        << right << setw(17 - strlen(name))
        << " lex="
        << '\'' << Sys::repr(tok.lex.beg, tok.lex.end) << '\''
        << right << setw(12 - (tok.lex.end - tok.lex.beg))
        << " ^" << Sys::repr(input_ptr, end);
    if (end < input_end) cout
        << "...";
    cout
        << "$" << endl;
}

#endif /* DEBUG */

} // namespace Base

namespace Num {

class Parser : private Base::Parser
{
public:
    typedef unsigned long long result_t;

    Parser(
#ifdef DEBUG
        bool _debug,
#endif
        const char *_input_ptr,
        const char *_input_end,
        size_t      _input_lno,
        bool        _verbose_error = false,
        bool        _free_spaces = false,
        const char *_tail_char = 0);

    result_t parse(bool strict = true, const char* input_ptr = 0);

    range_t get_prev_tok_lex() const
    { return prev_tok.lex; }

    range_t get_tok_lex() const
    { return tok.lex; }

private:
    typedef Base::Parser base_t;

    struct token_t
    {
        typedef base_t::token_t::type_t type_t;

        enum {
            bon        = base_t::token_t::bos,
            eon        = base_t::token_t::eos,

            million    = 1ULL << 2,  // "Million"
            millionen  = 1ULL << 3,  // "Millionen"
            acht       = 1ULL << 4,  // "acht"
            drei       = 1ULL << 5,  // "drei"
            ein        = 1ULL << 6,  // "ein"
            eine       = 1ULL << 7,  // "eine"
            eins       = 1ULL << 8,  // "eins"
            elf        = 1ULL << 9,  // "elf"
            fuenf      = 1ULL << 10, // "fuenf"
            hundert    = 1ULL << 11, // "hundert"
            neun       = 1ULL << 12, // "neun"
            null       = 1ULL << 13, // "null"
            sech       = 1ULL << 14, // "sech"
            sechs      = 1ULL << 15, // "sechs"
            sieb       = 1ULL << 16, // "sieb"
            sieben     = 1ULL << 17, // "sieben"
            ssig       = 1ULL << 18, // "ssig"
            tausend    = 1ULL << 19, // "tausend"
            und        = 1ULL << 20, // "und"
            vier       = 1ULL << 21, // "vier"
            zehn       = 1ULL << 22, // "zehn"
            zig        = 1ULL << 23, // "zig"
            zwan       = 1ULL << 24, // "zwan"
            zwei       = 1ULL << 25, // "zwei"
            zwoelf     = 1ULL << 26, // "zwoelf"

            milliarde  = 1ULL << 27, // "Milliarde"
            milliarden = 1ULL << 28, // "Milliarden"
            billion    = 1ULL << 29, // "Billion"
            billionen  = 1ULL << 30, // "Billionen"
            billiarde  = 1ULL << 31, // "Billiarde"
            billiarden = 1ULL << 32, // "Billiarden"
        };
    };

    static bool lookup_token(const char*, token_t::type_t&, const char*&);
    void next_token();

    result_t parse_numeral();
    result_t parse_million();
    result_t parse_million_base();
    result_t parse_tausend();
    result_t parse_tausend_base();
    result_t parse_hundert();
    result_t parse_hundert_base();
    result_t parse_zehn();
    result_t parse_zehn_base();
    result_t parse_zehn_atom();
    result_t parse_ss_long_atom();
    result_t parse_ss_root_atom();
    result_t parse_vfan_atom();

#ifdef DEBUG
    void print_id(std::ostream& ost) const
    { ost << (strict ? 'S' : 'N'); }
#endif

    static const char* const token_names[];

    const char* prev_ptr;
    bool strict;
};

const char* const Parser::token_names[] = {
    "BON",            // token_t::bon
    "EON",            // token_t::eon
    "\"Million\"",    // token_t::million
    "\"Millionen\"",  // token_t::millionen
    "\"acht\"",       // token_t::acht
    "\"drei\"",       // token_t::drei
    "\"ein\"",        // token_t::ein
    "\"eine\"",       // token_t::eine
    "\"eins\"",       // token_t::eins
    "\"elf\"",        // token_t::elf
    "\"fuenf\"",      // token_t::fuenf
    "\"hundert\"",    // token_t::hundert
    "\"neun\"",       // token_t::neun
    "\"null\"",       // token_t::null
    "\"sech\"",       // token_t::sech
    "\"sechs\"",      // token_t::sechs
    "\"sieb\"",       // token_t::sieb
    "\"sieben\"",     // token_t::sieben
    "\"ssig\"",       // token_t::ssig
    "\"tausend\"",    // token_t::tausend
    "\"und\"",        // token_t::und
    "\"vier\"",       // token_t::vier
    "\"zehn\"",       // token_t::zehn
    "\"zig\"",        // token_t::zig
    "\"zwan\"",       // token_t::zwan
    "\"zwei\"",       // token_t::zwei
    "\"zwoelf\"",     // token_t::zwoelf
    "\"Milliarde\"",  // token_t::milliarde
    "\"Milliarden\"", // token_t::milliarden
    "\"Billion\"",    // token_t::billion
    "\"Billionen\"",  // token_t::billionen
    "\"Billiarde\"",  // token_t::billiarde
    "\"Billiarden\""  // token_t::billiarden
};

Parser::Parser(
#ifdef DEBUG
    bool _debug,
#endif
    const char *_input_ptr,
    const char *_input_end,
    size_t      _input_lno,
    bool        _verbose_error,
    bool        _free_spaces,
    const char *_tail_char)
:
    base_t(
#ifdef DEBUG
        _debug,
#endif
        token_names,
        Sys::array_size(token_names),
        _input_ptr,
        _input_end,
        _input_lno,
        _verbose_error,
        _free_spaces,
        _tail_char),
    prev_ptr(input_ptr),
    strict(true)
{}

// $ calc-token -T -gn|calc-gen-func

bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
    // pattern: Billi(arde[n]|on[en])|Milli(arde[n]|on[en])|acht|drei|e(in[e|s]|lf)|fuenf|hundert|n(eun|ull)|s(ech[s]|ieb[en]|sig)|tausend|und|vier|z(ehn|ig|w(an|ei|oelf))
    switch (*n ++) {
    case 'B':
        if (*n ++ == 'i' &&
            *n ++ == 'l' &&
            *n ++ == 'l' &&
            *n ++ == 'i') {
            switch (*n ++) {
            case 'a':
                if (*n ++ == 'r' &&
                    *n ++ == 'd' &&
                    *n ++ == 'e') {
                    const char* p = n;
                    if (*n ++ == 'n') {
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
            case 'o':
                if (*n ++ == 'n') {
                    const char* p = n;
                    if (*n ++ == 'e' &&
                        *n ++ == 'n') {
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
    case 'M':
        if (*n ++ == 'i' &&
            *n ++ == 'l' &&
            *n ++ == 'l' &&
            *n ++ == 'i') {
            switch (*n ++) {
            case 'a':
                if (*n ++ == 'r' &&
                    *n ++ == 'd' &&
                    *n ++ == 'e') {
                    const char* p = n;
                    if (*n ++ == 'n') {
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
            case 'o':
                if (*n ++ == 'n') {
                    const char* p = n;
                    if (*n ++ == 'e' &&
                        *n ++ == 'n') {
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
    case 'a':
        if (*n ++ == 'c' &&
            *n ++ == 'h' &&
            *n ++ == 't') {
            r = n;
            t = token_t::acht;
            return true;
        }
        return false;
    case 'd':
        if (*n ++ == 'r' &&
            *n ++ == 'e' &&
            *n ++ == 'i') {
            r = n;
            t = token_t::drei;
            return true;
        }
        return false;
    case 'e':
        switch (*n ++) {
        case 'i':
            if (*n ++ == 'n') {
                const char* p = n;
                switch (*n ++) {
                case 'e':
                    r = n;
                    t = token_t::eine;
                    return true;
                case 's':
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
        case 'l':
            if (*n ++ == 'f') {
                r = n;
                t = token_t::elf;
                return true;
            }
        }
        return false;
    case 'f':
        if (*n ++ == 'u' &&
            *n ++ == 'e' &&
            *n ++ == 'n' &&
            *n ++ == 'f') {
            r = n;
            t = token_t::fuenf;
            return true;
        }
        return false;
    case 'h':
        if (*n ++ == 'u' &&
            *n ++ == 'n' &&
            *n ++ == 'd' &&
            *n ++ == 'e' &&
            *n ++ == 'r' &&
            *n ++ == 't') {
            r = n;
            t = token_t::hundert;
            return true;
        }
        return false;
    case 'n':
        switch (*n ++) {
        case 'e':
            if (*n ++ == 'u' &&
                *n ++ == 'n') {
                r = n;
                t = token_t::neun;
                return true;
            }
            return false;
        case 'u':
            if (*n ++ == 'l' &&
                *n ++ == 'l') {
                r = n;
                t = token_t::null;
                return true;
            }
        }
        return false;
    case 's':
        switch (*n ++) {
        case 'e':
            if (*n ++ == 'c' &&
                *n ++ == 'h') {
                const char* p = n;
                if (*n ++ == 's') {
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
        case 'i':
            if (*n ++ == 'e' &&
                *n ++ == 'b') {
                const char* p = n;
                if (*n ++ == 'e' &&
                    *n ++ == 'n') {
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
        case 's':
            if (*n ++ == 'i' &&
                *n ++ == 'g') {
                r = n;
                t = token_t::ssig;
                return true;
            }
        }
        return false;
    case 't':
        if (*n ++ == 'a' &&
            *n ++ == 'u' &&
            *n ++ == 's' &&
            *n ++ == 'e' &&
            *n ++ == 'n' &&
            *n ++ == 'd') {
            r = n;
            t = token_t::tausend;
            return true;
        }
        return false;
    case 'u':
        if (*n ++ == 'n' &&
            *n ++ == 'd') {
            r = n;
            t = token_t::und;
            return true;
        }
        return false;
    case 'v':
        if (*n ++ == 'i' &&
            *n ++ == 'e' &&
            *n ++ == 'r') {
            r = n;
            t = token_t::vier;
            return true;
        }
        return false;
    case 'z':
        switch (*n ++) {
        case 'e':
            if (*n ++ == 'h' &&
                *n ++ == 'n') {
                r = n;
                t = token_t::zehn;
                return true;
            }
            return false;
        case 'i':
            if (*n ++ == 'g') {
                r = n;
                t = token_t::zig;
                return true;
            }
            return false;
        case 'w':
            switch (*n ++) {
            case 'a':
                if (*n ++ == 'n') {
                    r = n;
                    t = token_t::zwan;
                    return true;
                }
                return false;
            case 'e':
                if (*n ++ == 'i') {
                    r = n;
                    t = token_t::zwei;
                    return true;
                }
                return false;
            case 'o':
                if (*n ++ == 'e' &&
                    *n ++ == 'l' &&
                    *n ++ == 'f') {
                    r = n;
                    t = token_t::zwoelf;
                    return true;
                }
            }
        }
    }
    return false;
}

void Parser::next_token()
{
#ifdef DEBUG
    if (debug)
        dump(__func__, enter_mark);
#endif

    // stev: The tokens "Million" and "Millionen" are special by
    // the fact that they must be bounded by non-empty whitespaces
    // or BON on left and non-empty whitespaces or EON on the right.
    // The same is true for "Milliarde[n]", "Billion[en]"
    // and "Billiarde[n]".
    static const token_t::type_t ws_toks =
        token_t::million   | token_t::millionen  |
        token_t::milliarde | token_t::milliarden |
        token_t::billion   | token_t::billionen  |
        token_t::billiarde | token_t::billiarden;

    const bool ws0 = prev_tok.type & ws_toks;
    const bool ws1 = tok.type & ws_toks;

    if (!free_spaces &&
        prev_tok.type != token_t::bon &&
        ws1 && tok.lex.beg == prev_ptr)
        space_needed(pos(tok.lex.beg), prev_tok.type, tok.type);
    else
    if (!free_spaces &&
        !ws0 && !ws1 && tok.lex.beg > prev_ptr)
        space_not_allowed(pos(prev_ptr), prev_tok.type, tok.type);

    prev_ptr = input_ptr;
    prev_tok = tok;

    const char* b = skip_spaces(input_ptr);
    SYS_ASSERT(b >= input_ptr && b <= input_end);

    if (!free_spaces &&
        (tok.type & ws_toks) &&
        b == input_ptr &&
        b < input_end &&
        !is_tail_char(*b))
        space_needed(pos(input_ptr), tok.type);

    const char* p = 0;
    token_t::type_t t = 0;
    if (b < input_end && lookup_token(b, t, p)) {
        SYS_ASSERT(p);
        SYS_ASSERT(p > b);
        SYS_ASSERT(p <= input_end);

        tok.lex = range_t(b, p);
        tok.type = t;
        input_ptr = p;
    }
    else
    if (b == input_end && tok.type == token_t::bon)
        empty_expr("numeral");
    else
    if (b == input_end && b > input_ptr && strict && !free_spaces)
        space_not_allowed(pos(input_ptr), tok.type, token_t::eon);
    else
    if ((b == input_end || !strict) && tok.type != token_t::bon) {
        tok.lex = range_t(input_ptr); 
        tok.type = token_t::eon;
        input_ptr = b;
    }
    else
        unexpected_text(b);

#ifdef DEBUG
    if (debug)
        dump(__func__, leave_mark);
#endif
}

Parser::result_t Parser::parse(bool strict, const char* input_ptr)
{
    if (input_ptr) {
        SYS_ASSERT(input_ptr >= input_beg);
        SYS_ASSERT(input_ptr <= input_end);
        this->input_ptr = input_ptr;
    }
    this->strict = strict;

#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    next_token();
    result_t res = parse_numeral();
    if (strict)
        need_token(token_t::eon, false);
    return res;
}

// numeral
//   : "null"
//   | "eins"
//   | million
//   ;
Parser::result_t Parser::parse_numeral()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    if (try_token(token_t::null))
        return 0;
    else
    if (try_token(token_t::eins))
        return 1;
    else
    if (peek_token(
        token_t::acht |
        token_t::drei |
        token_t::ein |
        token_t::eine |
        token_t::elf |
        token_t::fuenf |
        token_t::hundert |
        token_t::neun |
        token_t::sech |
        token_t::sechs |
        token_t::sieb |
        token_t::sieben |
        token_t::tausend |
        token_t::vier |
        token_t::zehn |
        token_t::zwan |
        token_t::zwei |
        token_t::zwoelf)) {
        return parse_million();
    }
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

// million
//   : "eine" "Million" [ million_base ]
//   | tausend [ "Millionen" [ million_base ] ]
//   ;
Parser::result_t Parser::parse_million()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    if (try_token(token_t::eine)) {
        need_token(token_t::million);
        result_t res = 1000000;
        if (peek_token(
            token_t::acht |
            token_t::drei |
            token_t::ein |
            token_t::eins |
            token_t::elf |
            token_t::fuenf |
            token_t::hundert |
            token_t::neun |
            token_t::sech |
            token_t::sechs |
            token_t::sieb |
            token_t::sieben |
            token_t::tausend |
            token_t::vier |
            token_t::zehn |
            token_t::zwan |
            token_t::zwei |
            token_t::zwoelf)) {
            res += parse_million_base();
        }
        return res;
    }
    else
    if (peek_token(
        token_t::acht |
        token_t::drei |
        token_t::ein |
        token_t::elf |
        token_t::fuenf |
        token_t::hundert |
        token_t::neun |
        token_t::sech |
        token_t::sechs |
        token_t::sieb |
        token_t::sieben |
        token_t::tausend |
        token_t::vier |
        token_t::zehn |
        token_t::zwan |
        token_t::zwei |
        token_t::zwoelf)) {
        result_t res = parse_tausend();
        if (try_token(token_t::millionen)) {
            res *= 1000000;
            if (peek_token(
                token_t::acht |
                token_t::drei |
                token_t::ein |
                token_t::eins |
                token_t::elf |
                token_t::fuenf |
                token_t::hundert |
                token_t::neun |
                token_t::sech |
                token_t::sechs |
                token_t::sieb |
                token_t::sieben |
                token_t::tausend |
                token_t::vier |
                token_t::zehn |
                token_t::zwan |
                token_t::zwei |
                token_t::zwoelf)) {
                res += parse_million_base();
            }
        }
        return res;
    }
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

// million_base
//   : "eins"
//   | tausend
//   ;
Parser::result_t Parser::parse_million_base()
{
    if (try_token(token_t::eins))
        return 1;
    else
    if (peek_token(
        token_t::acht |
        token_t::drei |
        token_t::ein |
        token_t::elf |
        token_t::fuenf |
        token_t::hundert |
        token_t::neun |
        token_t::sech |
        token_t::sechs |
        token_t::sieb |
        token_t::sieben |
        token_t::tausend |
        token_t::vier |
        token_t::zehn |
        token_t::zwan |
        token_t::zwei |
        token_t::zwoelf)) {
        return parse_tausend();
    }
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

// tausend
//   : "ein" ( ( zehn_base | hundert_base ) [ tausend_base ] | tausend_base )
//   | hundert [ tausend_base ]
//   | tausend_base
//   ;
Parser::result_t Parser::parse_tausend()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    if (try_token(token_t::ein)) {
        if (peek_token(
            token_t::hundert |
            token_t::und)) {
            result_t res;
            if (peek_token(token_t::und))
                res = 1 + parse_zehn_base();
            else
                res = 100 + parse_hundert_base();
            if (peek_token(token_t::tausend))
                res = res * 1000 + parse_tausend_base();
            return res;
        }
        else
            return 1000 + parse_tausend_base();
    }
    else
    if (peek_token(
        token_t::acht |
        token_t::drei |
        token_t::elf |
        token_t::fuenf |
        token_t::hundert |
        token_t::neun |
        token_t::sech |
        token_t::sechs |
        token_t::sieb |
        token_t::sieben |
        token_t::vier |
        token_t::zehn |
        token_t::zwan |
        token_t::zwei |
        token_t::zwoelf)) {
        result_t res = parse_hundert();
        if (peek_token(token_t::tausend))
            return res * 1000 + parse_tausend_base();
        else
            return res;
    }
    else
    if (peek_token(token_t::tausend))
        return 1000 + parse_tausend_base();
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

// tausend_base
//   : "tausend" [ "eins" | "ein" ( zehn_base | hundert_base ) | hundert ]
//   ;
Parser::result_t Parser::parse_tausend_base()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    need_token(token_t::tausend);
    if (peek_token(
        token_t::acht |
        token_t::drei |
        token_t::ein |
        token_t::eins |
        token_t::elf |
        token_t::fuenf |
        token_t::hundert |
        token_t::neun |
        token_t::sech |
        token_t::sechs |
        token_t::sieb |
        token_t::sieben |
        token_t::vier |
        token_t::zehn |
        token_t::zwan |
        token_t::zwei |
        token_t::zwoelf)) {
        if (try_token(token_t::eins))
            return 1;
        if (try_token(token_t::ein)) {
            if (peek_token(token_t::und))
                return 1 + parse_zehn_base();
            else
                return 100 + parse_hundert_base();
        }
        return parse_hundert();
    }
    return 0;
}

// hundert
//   : zehn_atom
//   | "zwei" [ zehn_base | hundert_base ]
//   | "drei" [ "zehn" | "ssig" | zehn_base | hundert_base ]
//   | vfan_atom [ "zehn" | "zig" | zehn_base | hundert_base ]
//   | ss_long_atom [ zehn_base | hundert_base ]
//   | hundert_base
//   ;
Parser::result_t Parser::parse_hundert()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    if (peek_token(
        token_t::elf |
        token_t::sech |
        token_t::sieb |
        token_t::zehn |
        token_t::zwan |
        token_t::zwoelf)) {
        return parse_zehn_atom();
    }
    else
    if (try_token(token_t::zwei)) {
        if (peek_token(
            token_t::hundert |
            token_t::und)) {
            if (peek_token(token_t::und))
                return 2 + parse_zehn_base();
            else
                return 200 + parse_hundert_base();
        }
        return 2;
    }
    else
    if (try_token(token_t::drei)) {
        if (peek_token(
            token_t::hundert |
            token_t::ssig |
            token_t::und |
            token_t::zehn)) {
            if (try_token(token_t::zehn))
                return 13;
            else
            if (try_token(token_t::ssig))
                return 30;
            else
            if (peek_token(token_t::und))
                return 3 + parse_zehn_base();
            else
                return 300 + parse_hundert_base();
        }
        return 3;
    }
    else
    if (peek_token(
        token_t::acht |
        token_t::fuenf |
        token_t::neun |
        token_t::vier)) {
        result_t atom = parse_vfan_atom();
        if (peek_token(
            token_t::hundert |
            token_t::und |
            token_t::zehn |
            token_t::zig)) {
            if (try_token(token_t::zehn))
                return atom + 10;
            else
            if (try_token(token_t::zig))
                return atom * 10;
            else
            if (peek_token(token_t::und))
                return atom + parse_zehn_base();
            else
                return atom * 100 + parse_hundert_base();
        }
        return atom;
    }
    else
    if (peek_token(
        token_t::sechs |
        token_t::sieben)) {
        result_t atom = parse_ss_long_atom();
        if (peek_token(
            token_t::hundert |
            token_t::und)) {
            if (peek_token(token_t::und))
                return atom + parse_zehn_base();
            else
                return atom * 100 + parse_hundert_base();
        }
        return atom;
    }
    else
    if (peek_token(token_t::hundert)) {
        return 100 + parse_hundert_base();
    }
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

// hundert_base
//   : "hundert" [ "eins" | zehn ]
//   ;
Parser::result_t Parser::parse_hundert_base()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    need_token(token_t::hundert);
    if (peek_token(
        token_t::acht |
        token_t::drei |
        token_t::ein |
        token_t::eins |
        token_t::elf |
        token_t::fuenf |
        token_t::neun |
        token_t::sech |
        token_t::sechs |
        token_t::sieb |
        token_t::sieben |
        token_t::vier |
        token_t::zehn |
        token_t::zwan |
        token_t::zwei |
        token_t::zwoelf)) {
        if (!try_token(token_t::eins)) {
            return parse_zehn();
        }
        return 1;
    }
    return 0;
}

// zehn
//   : zehn_atom
//   | "ein" zehn_base
//   | "zwei" [ zehn_base ]
//   | "drei" [ "zehn" | "ssig" | zehn_base ]
//   | vfan_atom [ "zehn" | "zig" | zehn_base ]
//   | ss_long_atom [ zehn_base ]
//   ;
Parser::result_t Parser::parse_zehn()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    if (peek_token(
        token_t::elf |
        token_t::sech |
        token_t::sieb |
        token_t::zehn |
        token_t::zwan |
        token_t::zwoelf)) {
        return parse_zehn_atom();
    }
    else
    if (try_token(token_t::ein)) {
        return 1 + parse_zehn_base();
    }
    else
    if (try_token(token_t::zwei)) {
        return peek_token(token_t::und)
            ? 2 + parse_zehn_base()
            : 2;
    }
    else
    if (try_token(token_t::drei)) {
        if (peek_token(
            token_t::ssig |
            token_t::und |
            token_t::zehn)) {
            if (try_token(token_t::zehn))
                return 13;
            else
            if (try_token(token_t::ssig))
                return 30;
            else
                return 3 + parse_zehn_base();
        }
        return 3;
    }
    else
    if (peek_token(
        token_t::acht |
        token_t::fuenf |
        token_t::neun |
        token_t::vier)) {
        result_t atom = parse_vfan_atom();
        if (peek_token(
            token_t::und |
            token_t::zehn |
            token_t::zig)) {
            if (try_token(token_t::zehn))
                return atom + 10;
            else
            if (try_token(token_t::zig))
                return atom * 10;
            else
                return atom + parse_zehn_base();
        }
        return atom;
    }
    else
    if (peek_token(
        token_t::sechs |
        token_t::sieben)) {
        result_t atom = parse_ss_long_atom();
        return peek_token(token_t::und)
            ? atom + parse_zehn_base()
            : atom;
    }
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

// zehn_base
//   : "und" ( "zwan" "zig" | "drei" "ssig" | vfan_atom "zig" | ss_root_atom "zig" )
//   ;
Parser::result_t Parser::parse_zehn_base()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    need_token(token_t::und);
    if (try_token(token_t::zwan)) {
        need_token(token_t::zig);
        return 20;
    }
    else
    if (try_token(token_t::drei)) {
        need_token(token_t::ssig);
        return 30;
    }
    else
    if (peek_token(
        token_t::acht |
        token_t::fuenf |
        token_t::neun |
        token_t::vier)) {
        result_t atom = parse_vfan_atom();
        need_token(token_t::zig);
        return atom * 10;
    }
    else
    if (peek_token(
        token_t::sech |
        token_t::sieb)) {
        result_t atom = parse_ss_root_atom();
        need_token(token_t::zig);
        return atom * 10;
    }
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

// zehn_atom
//   : "zehn"
//   | "elf"
//   | "zwoelf"
//   | "zwan" "zig"
//   | ss_root_atom ( "zehn" | "zig" )
//   ;
Parser::result_t Parser::parse_zehn_atom()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    if (try_token(token_t::zehn))
        return 10;
    else
    if (try_token(token_t::elf))
        return 11;
    else
    if (try_token(token_t::zwoelf))
        return 12;
    else
    if (try_token(token_t::zwan)) {
        need_token(token_t::zig);
        return 20;
    }
    else
    if (peek_token(
        token_t::sech |
        token_t::sieb)) {
        result_t atom = parse_ss_root_atom();
        if (try_token(token_t::zehn))
            return atom + 10;
        else {
            need_token(token_t::zig);
            return atom * 10;
        }
        return atom;
    }
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

// ss_long_atom
//   : "sechs"
//   | "sieben"
//   ;
Parser::result_t Parser::parse_ss_long_atom()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    if (try_token(token_t::sechs))
        return 6;
    else
    if (try_token(token_t::sieben))
        return 7;
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

// ss_root_atom
//   : "sech"
//   | "sieb"
//   ;
Parser::result_t Parser::parse_ss_root_atom()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    if (try_token(token_t::sech))
        return 6;
    else
    if (try_token(token_t::sieb))
        return 7;
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

// vfan_atom
//   : "vier"
//   | "fuenf"
//   | "acht"
//   | "neun"
//   ;
Parser::result_t Parser::parse_vfan_atom()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    if (try_token(token_t::vier))
        return 4;
    else
    if (try_token(token_t::fuenf))
        return 5;
    else
    if (try_token(token_t::acht))
        return 8;
    else
    if (try_token(token_t::neun))
        return 9;
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

} // namespace Num

namespace Expr {

class Parser : private Base::Parser
{
public:
    typedef long double result_t;

    Parser(
#ifdef DEBUG
        bool        _debug,
#endif
        const char* _input_str,
        size_t      _input_lno = 0,
        bool        _verbose_error = false,
        bool        _free_spaces = false);

    result_t parse();

private:
    typedef Base::Parser base_t;

    struct token_t
    {
        typedef base_t::token_t::type_t type_t;

        enum {
            boe   = base_t::token_t::bos,
            eoe   = base_t::token_t::eos,

            open  = 1ULL << 2,  // '('
            close = 1ULL << 3,  // ')'
            add   = 1ULL << 4,  // = "+" | "plus"
            sub   = 1ULL << 5,  // = "-" | "minus"
            mul   = 1ULL << 6,  // = "*" | "mul" | "mal"
            div   = 1ULL << 7,  // = "/" | "div"
            pow   = 1ULL << 8,  // = "^" | "power" | "hoch"
            lit   = 1ULL << 9,  // = [0-9]+
            num   = 1ULL << 10, // = parsed separately
        };
    };

    static bool is_char_token(char, token_t::type_t&);
    static bool lookup_token(const char*, token_t::type_t&, const char*&);

    void next_token();

    result_t parse_expr();
    result_t parse_term();
    result_t parse_factor();
    result_t parse_primary();

#ifdef DEBUG
    void print_id(std::ostream& ost) const
    { ost << 'E'; }
#endif

    static const char* const token_names[];

    typedef Num::Parser::result_t num_val_t;

    const char* prev_num_end;
    num_val_t   num_val;
};

const char* const Parser::token_names[] = {
    "BOE", // token_t::boe
    "EOE", // token_t::eoe
    "'('", // token_t::open
    "')'", // token_t::close
    "ADD", // token_t::add
    "SUB", // token_t::sub
    "MUL", // token_t::mul
    "DIV", // token_t::div
    "POW", // token_t::pow
    "LIT", // token_t::lit
    "NUM", // token_t::num
};

Parser::Parser(
#ifdef DEBUG
    bool        _debug,
#endif
    const char* _input_str,
    size_t      _input_lno,
    bool        _verbose_error,
    bool        _free_spaces)
:
    base_t(
#ifdef DEBUG
        _debug,
#endif
        token_names,
        Sys::array_size(token_names),
        _input_str,
        _input_str + strlen(_input_str),
        _input_lno,
        _verbose_error,
        _free_spaces,
        "()+-*/^0123456789"),
    prev_num_end(_input_str),
    num_val(0)
{}

inline bool Parser::is_char_token(char c, token_t::type_t& t)
{
    if (c == '(') {
        t = token_t::open;
        return true;
    }
    if (c == ')') {
        t = token_t::close;
        return true;
    }
    if (c == '+') {
        t = token_t::add;
        return true;
    }
    if (c == '-') {
        t = token_t::sub;
        return true;
    }
    if (c == '*') {
        t = token_t::mul;
        return true;
    }
    if (c == '/') {
        t = token_t::div;
        return true;
    }
    if (c == '^') {
        t = token_t::pow;
        return true;
    }
    return false;
}

// $ print() { printf '%s\n' "$@"; }
// $ print 'plus =add' 'minus =sub' mul 'mal =mul' div 'power hoch =pow'|calc-gen-func

bool Parser::lookup_token(const char* n, token_t::type_t& t, const char*& r)
{
    // pattern: div|hoch|m(al|inus|ul)|p(lus|ower)
    switch (*n ++) {
    case 'd':
        if (*n ++ == 'i' &&
            *n ++ == 'v') {
            r = n;
            t = token_t::div;
            return true;
        }
        return false;
    case 'h':
        if (*n ++ == 'o' &&
            *n ++ == 'c' &&
            *n ++ == 'h') {
            r = n;
            t = token_t::pow;
            return true;
        }
        return false;
    case 'm':
        switch (*n ++) {
        case 'a':
            if (*n ++ == 'l') {
                r = n;
                t = token_t::mul;
                return true;
            }
            return false;
        case 'i':
            if (*n ++ == 'n' &&
                *n ++ == 'u' &&
                *n ++ == 's') {
                r = n;
                t = token_t::sub;
                return true;
            }
            return false;
        case 'u':
            if (*n ++ == 'l') {
                r = n;
                t = token_t::mul;
                return true;
            }
        }
        return false;
    case 'p':
        switch (*n ++) {
        case 'l':
            if (*n ++ == 'u' &&
                *n ++ == 's') {
                r = n;
                t = token_t::add;
                return true;
            }
            return false;
        case 'o':
            if (*n ++ == 'w' &&
                *n ++ == 'e' &&
                *n ++ == 'r') {
                r = n;
                t = token_t::pow;
                return true;
            }
        }
    }
    return false;
}

void Parser::next_token()
{
#ifdef DEBUG
    if (debug)
        dump(__func__, enter_mark);
#endif

    prev_tok = tok;

    using namespace std;
    const char *ptr0 = skip_spaces();
    const char *ptr = input_ptr;
    const char* ptr2 = 0;
    if (ptr >= input_end) {
        tok.type = token_t::eoe;
        tok.lex = range_t(input_end);
    }
    else
    if (is_char_token(*ptr, tok.type)) {
        tok.lex = range_t(ptr, ptr + 1);
        input_ptr ++;
    }
    else
    if (islower(*ptr)) {
        if (!lookup_token(ptr, tok.type, ptr2))
            goto parse_num;

        SYS_ASSERT(ptr2);
        SYS_ASSERT(ptr2 > ptr);
        SYS_ASSERT(ptr2 <= input_end);

        tok.lex = range_t(ptr, ptr2);
        input_ptr = ptr2;
    }
    else
    if (isdigit(*ptr)) {
        errno = 0;
        num_val = strtoull(ptr,
            const_cast<char**>(&ptr2), 10);
        SYS_ASSERT(ptr2 > ptr);
        SYS_ASSERT(ptr2 <= input_end);

        if (errno)
            error("invalid literal value '%.*s'",
                Sys::integer_cast<int>(ptr2 - ptr), ptr);

        tok.type = token_t::lit;
        tok.lex = range_t(ptr, ptr2);
        input_ptr = ptr2;
    }
    else {
    parse_num:
        Num::Parser num(
#ifdef DEBUG
            debug,
#endif
            input_beg,
            input_end,
            input_lno,
            verbose_error,
            free_spaces,
            tail_char);

        // stev: we're entering an inner parser
        num_val = num.parse(
            ptr0 > input_beg &&
            ptr0 <= prev_num_end,
            ptr);

        range_t prev = num.get_prev_tok_lex();
        SYS_ASSERT(prev.beg >= ptr);
        SYS_ASSERT(prev.end >= prev.beg);
        SYS_ASSERT(prev.end <= input_end);

        range_t last = num.get_tok_lex();
        SYS_ASSERT(last.beg >= ptr);
        SYS_ASSERT(last.end >= last.beg);
        SYS_ASSERT(last.end <= input_end);
        SYS_ASSERT(prev.end <= last.beg);

        tok.type = token_t::num;
        tok.lex = range_t(ptr, prev.end);
        input_ptr = last.beg;

        prev_num_end = last.end;
        ptr2 = prev.end;
    }

    if (ptr2 && ptr2 < input_end &&
        !is_tail_char(*ptr2) &&
        !isblank(*ptr2) &&
        !free_spaces)
        space_needed(pos(ptr2), tok.type);

#ifdef DEBUG
    if (debug)
        dump(__func__, leave_mark);
#endif
}

Parser::result_t Parser::parse()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    next_token();
    if (peek_token(token_t::eoe))
        empty_expr();
    result_t res = parse_expr();
    need_token(token_t::eoe, false);
    return res;
}

// expr
//   : term ( ( ADD | SUB ) term )*
//   ;
Parser::result_t Parser::parse_expr()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    result_t res = parse_term();
    while (peek_token(
        token_t::add |
        token_t::sub)) {
        bool add = eat_token(token_t::add);
        result_t term = parse_term();
        if (add)
            res += term;
        else
            res -= term;
    }
    return res;
}

// term
//   : factor ( ( MUL | DIV ) factor )*
//   ;
Parser::result_t Parser::parse_term()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    result_t res = parse_factor();
    while (peek_token(
        token_t::div |
        token_t::mul)) {
        bool mul = eat_token(token_t::mul);
        result_t fac = parse_factor();
        if (mul)
            res *= fac;
        else
            res /= fac;
    }
    return res;
}

// factor
//   : primary [ POW factor ]
//   ;
Parser::result_t Parser::parse_factor()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    result_t base = parse_primary();
    if (try_token(token_t::pow)) {
        size_t pos = this->pos(prev_tok.lex.beg);
        result_t exp = parse_factor();
        errno = 0;
        result_t res = powl(base, exp);
        if (errno)
            error(pos, "power error: '%Lf' ^ '%Lf'", base, exp);
        return res;
    }
    return base;
}

// primary
//   : "(" expr ")"
//   | ( ADD | SUB ) term
//   | LIT
//   | NUM
//   ;
Parser::result_t Parser::parse_primary()
{
#ifdef DEBUG
    if (debug)
        dump(__func__);
#endif
    if (try_token(token_t::open)) {
        result_t res = parse_expr();
        need_token(token_t::close);
        return res;
    }
    else
    if (peek_token(
        token_t::add |
        token_t::sub)) {
        bool sub = eat_token(token_t::sub);
        result_t res = parse_term();
        if (sub) res = -res;
        return res;
    }
    else
    if (try_token(token_t::lit) ||
        try_token(token_t::num)) {
        return num_val;
    }
    else {
        missed_token(__func__);
        // make the compiler happy
        return 0;
    }
}

} // namespace Expr

class options_t
{
public:
    static const options_t* options(size_t argc, char* const argv[]);

    struct Error : public std::runtime_error
    {
        Error(const char* what) : std::runtime_error(what) {}
    };

    enum input_type_t {
        interact_input,
        args_input
    };
    enum parser_type_t {
        numeral_parser,
        expr_parser
    };

    input_type_t  input_type;
    parser_type_t parser_type;
#ifdef DEBUG
    bool          debug;
#endif
    bool          verbose;
    bool          free_spaces;
    size_t        argc;
    char *const  *argv;

private:
    options_t();
    ~options_t();

    typedef int opt_t;
    struct opt_type_t
    {
        enum {
            interact_input = 'i',
            args_input     = 'a',
            numeral_parser = 'N',
            expr_parser    = 'E',
            free_spaces    = 's',
            no_free_spaces = 256,
#ifdef DEBUG
            debug          = 'd',
            no_debug       = 'D',
#endif
            verbose        = 'V',
            help           = '?',
            version        = 'v',
            dump_options   = 257
        };
    };

    void parse(size_t argc, char* const argv[]);

    static void error(const char* msg, ...) PRINTF_FMT(1);
    static void invalid_opt_arg(const char* opt_name, const char* opt_arg);
    static void missing_opt_arg(const char* opt_name);
    static void missing_opt_arg(char opt_name);
    static void invalid_opt(const char* opt_name);
    static void invalid_opt(char opt_name);

    static void version();
    static void usage();

    void dump() const;

    options_t(const options_t&);
    options_t& operator=(const options_t&);
};

options_t::options_t() :
    input_type(args_input),
    parser_type(expr_parser),
#ifdef DEBUG
    debug(false),
#endif
    verbose(false),
    free_spaces(false),
    argc(0),
    argv(0)
{}

options_t::~options_t()
{}

void options_t::version()
{
    using namespace std;
    cout << program << ": version " << verdate << "\n\n" << license;
}

void options_t::usage()
{
    std::cout <<
        "usage: " << program << " [OPTION|INPUT]...\n"
        "the options are:\n"
        "  -i|--interact-input  input type: interactive\n"
        "  -a|--args-input      input type: command line arguments (default)\n"
        "  -N|--numeral-parser  parser type: numeral\n"
        "  -E|--expr-parser     parser type: expression (default)\n"
        "  -s|--free-spaces     let spaces be free separating numeral tokens\n"
        "     --no-free-spaces  or otherwise do not (default)\n"
#ifdef DEBUG
        "  -d|--debug           print some debugging output\n"
        "  -D|--no-debug        do not print debugging output (default)\n"
#endif
        "     --dump-options    print options and exit\n"
        "  -V|--verbose         be verbose\n"
        "  -v|--version         print version numbers and exit\n"
        "  -?|--help            display this help info and exit\n";
}

void options_t::dump() const
{
    static char const* noyes[] = {
        "no",
        "yes"
    };
    static char const* input_types[] = {
        "interact-input", // input_type_t::interact_input
        "args-input",     // input_type_t::args_input
    };
    static char const* parser_types[] = {
        "numeral-parser", // parser_type_t::numeral_parser
        "expr-parser",    // parser_type_t::expr_parser
    };
    using namespace std;
    using namespace Sys;
    cout
        << "input-type:  " << array(input_types)[input_type] << endl
        << "parser-type: " << array(parser_types)[parser_type] << endl
        << "free-spaces: " << array(noyes)[free_spaces] << endl
#ifdef DEBUG
        << "debug:       " << array(noyes)[debug] << endl
#endif
        << "verbose:     " << array(noyes)[verbose] << endl
        << "argc:        " << argc << endl;
    size_t i = 0;
    char *const *p = argv;
    for (; i < argc; i ++, p ++) {
        cout << "argv[" << i << left << setw(8 - Sys::digits(i)) << "]:";
        if (*p) cout << Sys::repr(*p);
        cout << endl;
    }
}

const options_t* options_t::options(size_t argc, char* const argv[])
{
    static options_t opts;
    opts.parse(argc, argv);
    return &opts;
}

void options_t::error(const char* msg, ...)
{
    va_list args;
    va_start(args, msg);
    Sys::verror<Error>(msg, args);
    //!!!VA_END va_end(args);
}

void options_t::invalid_opt_arg(const char* opt_name, const char* opt_arg)
{
    error("invalid argument for '%s' option: '%s'", opt_name, opt_arg);
}

void options_t::missing_opt_arg(const char* opt_name)
{
    error("argument for option '%s' not found", opt_name);
}

void options_t::missing_opt_arg(char opt_name)
{
    error("argument for option '-%c' not found", opt_name);
}

void options_t::invalid_opt(const char* opt_name)
{
    error("invalid command line option '%s'", opt_name);
}

void options_t::invalid_opt(char opt_name)
{
    error("invalid command line option '-%c'", opt_name);
}

void options_t::parse(size_t argc, char* const argv[])
{
    static const char short_opts[] =
        ":aEiNsvV"
#ifdef DEBUG
        "dD"
#endif
        ;
    static struct option long_opts[] = {
        { "interact-input", 0,       0, opt_type_t::interact_input },
        { "args-input",     0,       0, opt_type_t::args_input },
        { "numeral-parser", 0,       0, opt_type_t::numeral_parser },
        { "expr-parser",    0,       0, opt_type_t::expr_parser },
        { "free-spaces",    0,       0, opt_type_t::free_spaces },
        { "no-free-spaces", 0,       0, opt_type_t::no_free_spaces },
#ifdef DEBUG
        { "debug",          0,       0, opt_type_t::debug },
        { "no-debug",       0,       0, opt_type_t::no_debug },
#endif
        { "dump-options",   0,       0, opt_type_t::dump_options },
        { "verbose",        0,       0, opt_type_t::verbose },
        { "version",        0,       0, opt_type_t::version },
        { "help",           0, &optopt, opt_type_t::help },
        { 0,                0,       0, 0 },
    };

    struct bits_opts {
        unsigned dump: 1;
        unsigned usage: 1;
        unsigned version: 1;
    };
    struct bits_opts bits = {
        dump:    0,
        usage:   0,
        version: 0,
    };

    opt_t opt;
    optind = 1;
    opterr = 0;
    while ((opt = getopt_long(
        argc, argv, short_opts, long_opts, 0)) != EOF) {
        switch (opt) {
        case opt_type_t::interact_input:
            input_type = interact_input;
            break;
        case opt_type_t::args_input:
            input_type = args_input;
            break;
        case opt_type_t::numeral_parser:
            parser_type = numeral_parser;
            break;
        case opt_type_t::expr_parser:
            parser_type = expr_parser;
            break;
        case opt_type_t::free_spaces:
            free_spaces = true;
            break;
        case opt_type_t::no_free_spaces:
            free_spaces = false;
            break;
#ifdef DEBUG
        case opt_type_t::debug:
            debug = true;
            break;
        case opt_type_t::no_debug:
            debug = false;
            break;
#endif
        case opt_type_t::dump_options:
            bits.dump = 1;
            break;
        case opt_type_t::verbose:
            verbose = true;
            break;
        case opt_type_t::version:
            bits.version = 1;
            break;
        case ':': {
            const char* opt = argv[optind - 1];
            if (opt[0] == '-' && opt[1] == '-')
                missing_opt_arg(opt);
            else
                missing_opt_arg(optopt);
            break;
        }
        case 0:
            bits.usage = 1;
            break;
        case '?':
        default:
            if (optopt == 0)
                invalid_opt(argv[optind - 1]);
            else
            if (optopt != '?')
                invalid_opt(optopt);
            else
                bits.usage = 1;
            break;
        }
    }

    argv += optind;
    argc -= optind;

    this->argc = argc;
    this->argv = argv;

    if (bits.version)
        version();
    if (bits.dump)
        dump();
    if (bits.usage)
        usage();

    if (bits.dump ||
        bits.version ||
        bits.usage)
        exit(0);
}

inline void print_error(
    std::ostream& ost, const Base::ParserError& err)
{ ost << err.msg(program) << std::endl; }

inline void print_error(
    std::ostream& ost, const std::exception& exc)
{ ost << program << ": error: " << exc.what() << std::endl; }

class parse_op_t
{
public:
    typedef void (*func_t)(
        const char* str, size_t lno, const options_t* opts);

    parse_op_t(func_t _func, const options_t* _opts) :
        func(_func),
        opts(_opts),
        lno(0),
        err(0)
    {}

    void operator()(const char* str)
    {
        try {
            func(str, ++ lno, opts);
        }
        catch (const Base::ParserError& exc) {
            print_error(std::cerr, exc);
            err ++;
        }
    }

    const options_t* opt() const
    { return opts; }

    size_t err_count() const
    { return err; }

private:
    func_t func;
    const options_t* opts;
    size_t lno;
    size_t err;
};

template<typename T>
void parse(
    const char* str, size_t lno, const options_t* opts);

template<>
void parse<Num::Parser>(
    const char* str, size_t lno, const options_t* opts)
{
    using Num::Parser;
    using namespace std;

    Parser parser(
#ifdef DEBUG
        opts->debug,
#endif
        str,
        str + strlen(str),
        lno,
        opts->verbose,
        opts->free_spaces);

    cout << parser.parse() << endl;
}

template<>
void parse<Expr::Parser>(
    const char* str, size_t lno, const options_t* opts)
{
    using Expr::Parser;
    using namespace std;

    Parser parser(
#ifdef DEBUG
        opts->debug,
#endif
        str, lno,
        opts->verbose,
        opts->free_spaces);

    cout << fixed << parser.parse() << endl;
}

void input_interact(parse_op_t& op)
{
    using namespace std;

    // stev: assuming sync with stdio
    iostream::sync_with_stdio(true);

    bool tty = isatty(fileno(stdin));
    string str;

    while (1) {
        if (tty) cout << "> ";
        getline(cin, str);
        if (cin.rdstate() & (ios::eofbit | ios::failbit)) {
            if (tty) cout << endl;
            break;
        }
        if (tty && str.size() == 0)
            break;

        op(str.c_str());
    }
}

void input_args(parse_op_t& op)
{
    char* const* ptr = op.opt()->argv;
    char* const* end = ptr + op.opt()->argc;
    for (; ptr < end; ptr ++)
        op(*ptr);
}

int main(int argc, char* const argv[])
try
{
    static const parse_op_t::func_t parsers[] = {
        parse<Num::Parser>,  // options_t::numeral_parser
        parse<Expr::Parser>  // options_t::expr_parser
    };
    static void (*const inputs[])(parse_op_t&) = {
        input_interact,      // options_t::interact_input
        input_args           // options_t::args_input
    };

    const options_t* opts = options_t::options(argc, argv);
    parse_op_t op(Sys::array(parsers)[opts->parser_type], opts);
    Sys::array(inputs)[opts->input_type](op);

    return op.err_count();
}
catch (const std::exception& exc) {
    print_error(std::cerr, exc);
    return -1;
}

