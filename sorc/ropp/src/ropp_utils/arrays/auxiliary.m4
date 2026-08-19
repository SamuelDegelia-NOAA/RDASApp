divert(-1)dnl
ifdef(`NUMDIMS',,
	`errprint(`****NUMDIMS should be defined as 0, 1, 2, 3, 4, 5, 6, 7, ...****')m4exit')dnl
dnl
dnl
ifdef(`KINDVALUE',,
	`errprint(`****KINDVALUE must be defined as  "Text", "OneByteInt", "TwoByteInt", "FourByteInt", "EightByteInt", "FourByteReal", or "EightByteReal"****')m4exit')dnl
dnl
dnl
dnl# NCOLONS(1) = ":", NCOLONS(2) = ":, :", etc.
define(`NCOLONS',`ifelse($1, 1, `:', `:, '`NCOLONS(decr($1))')')
define(`COLONS',`NCOLONS(NUMDIMS)')
dnl
dnl
dnl# NSIZES(1) = "size(array,1)", NSIZES(2) = "size(array,1),size(array,2)", etc.
define(`NSIZES',`ifelse($1, 1, `size(array, $1)', `NSIZES(decr($1)), '`size(array, $1)')')
define(`SIZES',`NSIZES(NUMDIMS)')
dnl
dnl
dnl# NCOPY_ARGS(1) = "1:min(cnts(1),size(array,1))",
dnl# NCOPY_ARGS(2) = "1:min(cnts(1),size(array,1)), &
dnl#                  1:min(cnts(2),size(array,2))", etc.
define(`NCOPY_ARGS',`ifelse($1, 1, `1:min(cnts($1),size(array, $1))',dnl
`NCOPY_ARGS(decr($1)), &
'`           1:min(cnts($1),size(array, $1))')')
define(`COPY_ARGS',`ifelse(NUMDIMS, 1, `1:min(cnts,size(array))',`NCOPY_ARGS(NUMDIMS)')')
dnl
dnl
dnl# NALLOC_ARGS(1) = "cnts(1)", ALLOC_ARGS(2) = "cnts(1), cnts(2)", etc.
define(`NALLOCARGS',`ifelse($1, 1, `cnts($1)', `NALLOCARGS(decr($1)), '`cnts($1)')')
define(`ALLOC_ARGS',`ifelse(NUMDIMS, 1, `cnts',`NALLOCARGS(NUMDIMS)')')
dnl
dnl
dnl# NIDX_ARGS(1) = "idx(1)", IDX_ARGS(2) = "idx(1), idx(2)", etc.
define(`IDXARG',`ifelse($1, $2, `size(array,$2):1:-1', `:')')
define(`NIDXARGS',`ifelse($1, 1, `IDXARG($1,$2)', `NIDXARGS(decr($1),$2), '`IDXARG($1,$2)')')
define(`IDX_ARGS',`NIDXARGS(NUMDIMS,$1)')
dnl
dnl
dnl# NDIM_CNTS(1) = "             ", NDIM_CNTS(2) = "dimension(2),", etc.
define(`NDIM_CNTS',`ifelse($1, 1, `             ', `dimension($1),')')
define(`DIM_CNTS',`NDIM_CNTS(NUMDIMS)')
dnl
dnl
define(`NUMERIC_DECL',`$1(kind = KINDVALUE)')
define(`TEXT_DEFINES',
	`define(`TYPE',`character(len = *)')define(`OTYPE',`character(len = 1024)')define(`NCKIND',`Text')')
define(`INT1_DEFINES',
	`define(`TYPE',`NUMERIC_DECL(integer)')define(`OTYPE',`NUMERIC_DECL(integer)')define(`NCKIND',`int1')')
define(`INT2_DEFINES',
	`define(`TYPE',`NUMERIC_DECL(integer)')define(`OTYPE',`NUMERIC_DECL(integer)')define(`NCKIND',`int2')')
define(`INT4_DEFINES',
	`define(`TYPE',`NUMERIC_DECL(integer)')define(`OTYPE',`NUMERIC_DECL(integer)')define(`NCKIND',`int')')
define(`FLT4_DEFINES',
	`define(`TYPE',`NUMERIC_DECL(real)')define(`OTYPE',`NUMERIC_DECL(real)')define(`NCKIND',`real')')
define(`FLT8_DEFINES',
	`define(`TYPE',`NUMERIC_DECL(real)')define(`OTYPE',`NUMERIC_DECL(real)')define(`NCKIND',`double')')
ifelse(KINDVALUE,Text,`TEXT_DEFINES',
       KINDVALUE,OneByteInt,`INT1_DEFINES',
       KINDVALUE,TwoByteInt,`INT2_DEFINES',
       KINDVALUE,FourByteInt,`INT4_DEFINES',
       KINDVALUE,EightByteInt,`INT8_DEFINES',
       KINDVALUE,FourByteReal,`FLT4_DEFINES',
       KINDVALUE,EightByteReal,`FLT8_DEFINES',
       
	`errprint(`****KINDVALUE must be "Text", "OneByteInt", "TwoByteInt", "FourByteInt", "EightByteInt", "FourByteReal", or "EightByteReal"****')')
define(`ND_KINDVALUE',NUMDIMS`'D_`'KINDVALUE)
define(`COPY_ALLOC',`copy_alloc_'ND_KINDVALUE)
define(`REVERSE',`reverse_'ND_KINDVALUE)
define(`CALLOCATE',`callocate_'ND_KINDVALUE)
define(`ISINRANGE',`isinrange_'ND_KINDVALUE)
define(`REALLOCATE',`reallocate_'ND_KINDVALUE)
define(`PREALLOCATE',`preallocate_'ND_KINDVALUE)
define(`m4_rename',`ifdef(`$1',`define(`m4'_`$1',defn(`$1'))undefine(`$1')')')
m4_rename(`index')
m4_rename(`len')
m4_rename(`shift')
divert`'dnl
