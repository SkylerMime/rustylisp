# Initial Grammar for CI_LISP project

## Link
https://github.com/dempseyucla/232_CILISP/blob/master/instructions/task_1.md

## Grammar

```
program ::= s_expr EOL | s_expr EOFT | EOL | EOFT

s_expr ::= f_expr | number | QUIT

f_expr ::= ( FUNC s_expr_section )

s_expr_section ::= s_expr_list | <empty>

s_expr_list ::= s_expr | s_expr s_expr_list

FUNC ::= neg | abs | add | sub |
	mult | div | remainder | exp |
	exp2 | pow | log | sqrt |
	cbrt | hypot | max | min

number ::= INT | DOUBLE

INT ::= optional +/-,
	then some digits

DOUBLE ::= optional +/-,
	then some digits,
	then a decimal point,
	then optionally some more digits

QUIT ::= quit
```
