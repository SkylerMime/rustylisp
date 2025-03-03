# Initial Grammar for CI_LISP project

## Link
https://github.com/dempseyucla/232_CILISP/blob/master/instructions/task_1.md

## Grammar

```
program ::= s_expr EOL | s_expr EOFT | EOL | EOFT

s_expr ::= f_expr | number | SYMBOL | ( let_section s_expr ) | QUIT

f_expr ::= ( FUNC s_expr_section )

s_expr_section ::= s_expr_list | <empty>

s_expr_list ::= s_expr | s_expr s_expr_list

let_section ::= ( LET let_list )

let_list ::= let_elem | let_elem let_list

let_elem ::= ( SYMBOL s_expr )

SYMBOL ::= at least 1 letter, followed by 0 or more letters and digits

FUNC ::= neg | abs | add | sub |
	mult | div | remainder | exp |
	exp2 | pow | log | sqrt |
	cbrt | hypot | max | min

number ::= INT | DOUBLE

letter ::= lowercase english alphabet letter
	| uppercase english alphabet letter
	| $ | _

INT ::= optional +/-,
	then some digits

DOUBLE ::= optional +/-,
	then some digits,
	then a decimal point,
	then optionally some more digits

LET ::= let

QUIT ::= quit
```
