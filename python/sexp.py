# inspired by:
# pyparsing example sexpParser.py
# note: only using a small subset
#
# Demonstration of the pyparsing module, implementing a simple S-expression
# parser.
#
# Updates:
#  November, 2011 - fixed errors in precedence of alternatives in simpleString;
#      fixed exception raised in verifyLen to properly signal the input string 
#      and exception location so that markInputline works correctly; fixed 
#      definition of decimal to accept a single '0' and optional leading '-'
#      sign; updated tests to improve parser coverage
#
# Copyright 2007-2011, by Paul McGuire
# Copyright (c) 2003,2004  Paul T. McGuire
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import pyparsing
import string

sexpReader = pyparsing.Forward()
qstring = pyparsing.QuotedString(quoteChar='"',escChar='\\')
decimal = pyparsing.Regex(r'-?0|[1-9]\d*').setParseAction(lambda t: int(t[0])) # maybe always return float?
real = pyparsing.Regex(r"[+-]?\d+\.\d*([eE][+-]?\d+)?").setParseAction(lambda tokens: float(tokens[0]))
symbol = pyparsing.Word(pyparsing.alphanums + "-./_:*+=")
boolTrue = pyparsing.Literal("#t").setParseAction( pyparsing.replaceWith(True) )
boolFalse = pyparsing.Literal("#f").setParseAction( pyparsing.replaceWith(False) )
bool = boolTrue | boolFalse
nan = pyparsing.Literal("+nan.0").setParseAction( pyparsing.replaceWith(float("NaN")) )
literal = nan | real | decimal | qstring | symbol | bool
# define punctuation literals
LPAR, RPAR = map(pyparsing.Suppress, "()")
sexpList = pyparsing.Group(LPAR + pyparsing.ZeroOrMore(sexpReader) + RPAR)
sexpReader << ( literal | sexpList )

# todo: quite slow?!
def sexpReadFromString(s):
    return sexpReader.parseString(s).asList()[0]

# todo: quick hack
def sexpWriteToString(l):
    if isinstance(l,type('')):
        return l # todo: maybe quote?! but how to handle symbols then?!
    elif isinstance(l,type(1)) or isinstance(l,type(1.1)):
        return str(l)
    elif isinstance(l,type([])):
        return "("+string.join(map(lambda x: sexpWriteToString(x), l))+")"
    else:
        raise Exception("unsupported type",type(l),l)
