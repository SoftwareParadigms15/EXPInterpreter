# EXPInterpreter [![Build Status](https://travis-ci.org/SoftwareParadigms15/EXPInterpreter.svg?branch=master)](https://travis-ci.org/SoftwareParadigms15/EXPInterpreter)

## Description
This Scala programm can interpret any EXP programm.

## Usage
Compile and run the project with maven.

## EXP
EXP is a custom, turing complete programming language, which is used by the Software Paradigms course at the TU Graz. 
## Supported variables and functions
Following built-in variables are supported:
 - integer
 - list
 
Following built-in functions are supported:
 - plus(x,y)      (x+y)
 - minus(x,y)     (x-y)
 - mult(x,y)      (x*y)
 - div(x,y)       (x/y)
 - first(x)       (first element in list x)
 - rest(x)        (all elements in list x except the first one)
 - build(x,y)     (put element x in list y)
 - inc(x)         (increase x by one)
 - dec(x)         (decrease x by one)
 - reverse(x)     (returns list x in reverse order)
 - len(x)         (returns the length of list x as an integer)
 - fak(x)         (returns factorial of x)
 - sqrt(x)        (returns the square root of x, floors if necessary)
 - abs(x)         (returns the absolute value of x)

Following built-in conditionals are supported (all conditionals support lists and integers):
 - eq?(x,y)       (true if x and y have the same value)
 - lt?(x,y)       (true if x < y)
 - gt?(x,y)       (true if y > x)
 
User defined functions and exceptions are also supported. Built-in functions can be overloaded by user defined functions if they have the same name. In this case, always the user defined function will be used.

## Examples

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
Programm for counting the elements in a list:
```
{len(x) = if eq?(x,[]) then 0 else plus(1, len(rest(x)))}
len([1,2,3])
```
