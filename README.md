# EXPInterpreter

## Description
This Scala programm can interpret any EXP programm.

## EXP
EXP is a custom, turing complete programming language, which is used by the Software Paradigms course at the TU Graz. 

##Supported variables and functions
Following built in variables are supported:
 - int
 - list
 
Following built-in functions are supported:
 - plus
 - minus
 - mult
 - div
 - first
 - rest
 - build
 - eq?
 
User defined functions and exceptions are also supported.

##Examples

Basic example:

```
{}
if eq?([1],build(1,[])) then plus(2, mult(3,4)) else div(4,2)
```

User defined function:

```
{concat(xs, ys) = if eq?(xs,[]) then ys else build(first(xs), concat(rest(xs),ys))}
```

Programm with user defined functions and user defined exceptions:

```
{foo(a,b)= if eq?(a,b) then throw MyException else plus(a,b);
  bar(a) = try foo(1,1) catch {MyException : a ; _ : 7}
}
bar(5)
```

