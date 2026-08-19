divert(-1)dnl
dnl
dnl AUTHOR
dnl    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
dnl
dnl COPYRIGHT
dnl
dnl    Copyright (c) 2005 Christian Marquardt        <christian@marquardt.sc>
dnl
dnl    All rights reserved.
dnl
dnl    Permission is hereby granted, free of charge, to any person obtaining
dnl    a copy of this software and associated documentation files (the
dnl    "Software"), to deal in the Software without restriction, including
dnl    without limitation the rights to use, copy, modify, merge, publish,
dnl    distribute, sublicense, and/or sell copies of the Software, and to
dnl    permit persons to whom the Software is furnished to do so, subject to
dnl    the following conditions:
dnl
dnl    The above copyright notice and this permission notice shall be
dnl    included in all copies or substantial portions of the Software as well
dnl    as in supporting documentation.
dnl
dnl    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
dnl    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
dnl    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
dnl    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
dnl    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
dnl    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
dnl    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
dnl
ifdef(`NUMDIMS',,
	`errprint(`****NUMDIMS should be defined as 0, 1, 2, 3, 4, 5, 6, 7, ...****')m4exit')dnl
ifdef(`KINDVALUE',,
	`errprint(`****KINDVALUE must be defined as  "text", "OneByteInt", "TwoByteInt", "FourByteInt", "EightByteReal", "FourByteReal", or "EightByteReal"****')m4exit')dnl
ifdef(`PUTORGET',,
	`errprint(`****PUTORGET must be defined as "put" or "get"****')m4exit')dnl
dnl# NCOLONS(1) = ":", NCOLONS(2) = ":, :", etc.
define(`NCOLONS',`ifelse($1, 1, `:', `:, '`NCOLONS(decr($1))')')
define(`COLONS',`NCOLONS(NUMDIMS)')
dnl# ALLOC_ARGS(1) = "cnts(1)", ALLOC_ARGS(2) = "cnts(1), cnts(2)", etc.
define(`NALLOCARGS',`ifelse($1, 1, `cnts($1)', `NALLOCARGS(decr($1)), '`cnts($1)')')
define(`ALLOC_ARGS',`NALLOCARGS(NUMDIMS)')
ifelse(POINTER,`yes',
            `define(`INTENT_OR_POINTER',`pointer')',
            `define(`INTENT_OR_POINTER',`intent('IN_OR_OUT`)')')
ifelse(POINTER,`yes',
            `define(`OUTENT_OR_POINTER',`pointer')',
            `define(`OUTENT_OR_POINTER',`intent('OUT_OR_IN`)')')
define(`NUMERIC_DECL',`$1(kind = KINDVALUE)')
define(`TEXT_DEFINES',
	`define(`TYPE',`character (len = *)')define(`NCKIND',`text')')
define(`INT1_DEFINES',
	`define(`TYPE',`NUMERIC_DECL(integer)')define(`NCKIND',`int1')')
define(`INT2_DEFINES',
	`define(`TYPE',`NUMERIC_DECL(integer)')define(`NCKIND',`int2')')
define(`INT4_DEFINES',
	`define(`TYPE',`NUMERIC_DECL(integer)')define(`NCKIND',`int')')
define(`INT8_DEFINES',
	`define(`TYPE',`NUMERIC_DECL(integer)')define(`NCKIND',`int8')')
define(`FLT4_DEFINES',
	`define(`TYPE',`NUMERIC_DECL(real)')define(`NCKIND',`real')')
define(`FLT8_DEFINES',
	`define(`TYPE',`NUMERIC_DECL(real)')define(`NCKIND',`double')')
ifelse(KINDVALUE,text,`TEXT_DEFINES',
       KINDVALUE,OneByteInt,`INT1_DEFINES',
       KINDVALUE,TwoByteInt,`INT2_DEFINES',
       KINDVALUE,FourByteInt,`INT4_DEFINES',
       KINDVALUE,EightByteInt,`INT8_DEFINES',
       KINDVALUE,FourByteReal,`FLT4_DEFINES',
       KINDVALUE,EightByteReal,`FLT8_DEFINES',
       
	`errprint(`****KINDVALUE must be "text", "OneByteInt", "TwoByteInt", "FourByteInt", "EightByteReal", "FourByteReal", or "EightByteReal"****')')
ifelse(PUTORGET,`put',`define(`IN_OR_OUT',` in')',
       PUTORGET,`get',`define(`IN_OR_OUT',`out')',
       
	`errprint(`****PUTORGET must be "put" or "get"****')')
ifelse(PUTORGET,`put',`define(`OUT_OR_IN',`out')',
       PUTORGET,`get',`define(`OUT_OR_IN',` in')',
       
	`errprint(`****PUTORGET must be "put" or "get"****')')
define(`ND_KINDVALUE',NUMDIMS`'D_`'KINDVALUE)
ifelse(POINTER,`yes',
            `define(`NCDF_AFUN',`ncdf_'PUTORGET`val_'ND_KINDVALUE)',
            `define(`NCDF_AFUN',`ncdf_'PUTORGET`var_'ND_KINDVALUE)')
define(`UT_SFUN',`ut_converts_'KINDVALUE)
ifelse(POINTER,`yes',
            `define(`UT_AFUN',`ut_convertp_'ND_KINDVALUE)',
            `define(`UT_AFUN',`ut_converta_'ND_KINDVALUE)')
define(`m4_rename',`ifdef(`$1',`define(`m4'_`$1',defn(`$1'))undefine(`$1')')')
m4_rename(`index')
m4_rename(`len')
m4_rename(`shift')
divert`'dnl
