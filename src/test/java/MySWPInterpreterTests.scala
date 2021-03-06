import net.thewalkingthread.exp.interpreter.SWPInterpreter
import org.scalatest.FunSuite

class MySWPInterpreterTests extends FunSuite {

  test("Parser short program") {
    val prog = """
    {concat(xs, ys)=if eq?(xs,[]) then ys else build(first(xs), concat(rest(xs),ys))}
    concat([1,2,3],[4,5])
               """
    assert(SWPInterpreter.checkProgramGrammer(prog))
  }

  test("Parser defect program") {
    val prog = """
    {concat(xs, ys)=if eq?(xs,[] then ys else build(first(xs), concat(rest(xs),ys))}
    concat([1,2,3],[4,5])
               """
    assert(! SWPInterpreter.checkProgramGrammer(prog))
  }

  test("Interpreter program without functions") {
    val prog = """
    {}
    if eq?([1],build(1,[])) then plus(2, mult(3,4)) else div(4,2)
               """
    assertResult("14"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }


  test("Interpreter short program") {
    val prog = """
    {concat(xs, ys)=if eq?(xs,[]) then ys else build(first(xs), concat(rest(xs),ys))}
    concat([1,2,3],[4,5])
               """
    assertResult("[1,2,3,4,5]"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }

  test("DivByZero Exception") {
    val prog = """
    {foo(a,b)=if eq?(2, div(a,b)) then 1 else 0;
     bar(a) = mult(a, foo(2,0))
    }
    bar(5)
               """
    assertResult("Uncaught exception DivByZero at function div!"){
      SWPInterpreter.evaluateProgram(prog)
    }

  }

  test("TypeMismatch Exception") {
    val prog = """
    {
    }
    try plus(1,[]) catch {_ : 0}
               """
    assertResult("0"){
      SWPInterpreter.evaluateProgram(prog)
    }

  }
  test("TypeMismatch Exception1") {
    val prog = """
    {
    }
    try minus(1,[]) catch {_ : 0}
               """
    assertResult("0"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("TypeMismatch Exception2") {
    val prog = """
    {
    }
    try mult(1,[]) catch {_ : 0}
               """
    assertResult("0"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("TypeMismatch Exception3") {
    val prog = """
    {
    }
    try div(1,[]) catch {_ : 0}
               """
    assertResult("0"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("TypeMismatch Exception4") {
    val prog = """
    {
    }
    try plus([],2) catch {_ : 0}
               """
    assertResult("0"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("TypeMismatch Exception5") {
    val prog = """
    {
    }
    try minus([],2) catch {_ : 0}
               """
    assertResult("0"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("TypeMismatch Exception6") {
    val prog = """
    {
    }
    try mult([],2) catch {_ : 0}
               """
    assertResult("0"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("TypeMismatch Exception7") {
    val prog = """
    {
    }
    try div([],2) catch {_ : 0}
               """
    assertResult("0"){
      SWPInterpreter.evaluateProgram(prog)
    }
  }

  test("Custom Exception") {
    val prog = """
    {foo(a,b)= if eq?(a,b) then throw MyException else plus(a,b);
     bar(a) = try foo(1,1) catch {MyException : a ; _ : 7}
    }
    bar(5)
               """
    assertResult("5"){
      SWPInterpreter.evaluateProgram(prog)
    }

  }

  test("Test build") {
    val prog = """
    {foo(a,b) = build(a,b)}
    foo(245,[134,2,3,4,5,6,7,8])
               """
    assertResult("[245,134,2,3,4,5,6,7,8]") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test build1") {
    val prog = """
    {foo(a,b) = build(a,b)}
    foo(245,1)
               """
    assertResult("Uncaught exception TypeMismatch at function build!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test build2") {
    val prog = """
    {foo(a,b) = build(a,b)}
    foo([245],1)
               """
    assertResult("Uncaught exception TypeMismatch at function build!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test exception sequence 1") {
    val prog = """
    {foo(a,b) = build(a,div(a,b))}
    foo([245],0)
               """
    assertResult("Uncaught exception TypeMismatch at function div!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test exception sequence 2") {
    val prog = """
    {foo(a,b) = build(a,div(8,b))}
    foo([245],0)
               """
    assertResult("Uncaught exception DivByZero at function div!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test exception sequence 3") {
    val prog = """
    {foo(a,b) = build(a,div(b,0))}
    foo(245,2)
               """
    assertResult("Uncaught exception DivByZero at function div!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test exception sequence 4") {
    val prog = """
    {foo(a,b) = rest([0,1,div(a,b)])}
    foo([245],0)
               """
    assertResult("Uncaught exception TypeMismatch at function div!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test exception sequence 5") {
    val prog = """
    {foo(a,b) = rest([0,1,div(a,b)])}
    foo(245,0)
               """
    assertResult("Uncaught exception DivByZero at function div!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("rest 1") {
    val prog = """
    {foo(a,b) = rest([0,1,div(a,b)])}
    foo(45,5)
               """
    assertResult("[1,9]") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }

  test("Test first") {
    val prog = """
    {foo(a,b) = first([[plus(a,b), minus(1,1)], 2, 3, 4]) }
    foo(1,2)
               """
    assertResult("[3,0]") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test first1") {
    val prog = """
    {foo(a) = first(a) }
    foo([1,2,3,4,5])
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test firstTypeMismatch") {
    val prog = """
    {foo(a) = first(a) }
    foo(1)
               """
    assertResult("Uncaught exception TypeMismatch at function first!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test firstNil") {
    val prog = """
    {foo(a) = first(a) }
    foo([])
               """
    assertResult("Uncaught exception EmptyList at function first!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test plus") {
    val prog = """
    {foo(a,b) = plus(a,b) }
    foo(1,2)
               """
    assertResult("3") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test minus") {
    val prog = """
    {foo(a,b) = minus(a,b) }
    foo(7,15)
               """
    assertResult("-8") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test mult") {
    val prog = """
    {foo(a,b) = mult(a,b) }
    foo(3,500)
               """
    assertResult("1500") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test div") {
    val prog = """
    {foo(a,b) = div(a,b) }
    foo(12,4)
               """
    assertResult("3") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test eq0") {
    val prog = """
    {foo(a,b) = if eq?(a,b) then 1 else 0 }
    foo(12,4)
               """
    assertResult("0") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test eq1") {
    val prog = """
    {foo(a,b) = if eq?(a,b) then 1 else 0 }
    foo(12,12)
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test lt0") {
    val prog = """
    {foo(a,b) = if lt?(a,b) then 1 else 0 }
    foo(12,4)
               """
    assertResult("0") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test lt1") {
    val prog = """
    {foo(a,b) = if lt?(a,b) then 1 else 0 }
    foo(12,400)
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test rest") {
    val prog = """
    {foo(a) = rest(a) }
    foo([12,13,14,19])
               """
    assertResult("[13,14,19]") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test restTypeMismatch") {
    val prog = """
    {foo(a) = rest(a) }
    foo(1)
               """
    assertResult("Uncaught exception TypeMismatch at function rest!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test restNil") {
    val prog = """
    {foo(a) = rest(a) }
    foo([])
               """
    assertResult("Uncaught exception EmptyList at function rest!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test divby0") {
    val prog = """
    {foo(a,b) = div(a,b) }
    foo(12,0)
               """
    assertResult("Uncaught exception DivByZero at function div!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test exceptions sequence 6") {
    val prog = """
    {foo(a,b) = plus([1],div(a,b)) }
    foo(12,0)
               """
    assertResult("Uncaught exception DivByZero at function div!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test exception sequence 7") {
    val prog = """
    {foo(a,b) = rest([0,1,div(a,b),plus([],[])])}
    foo(245,0)
               """
    assertResult("Uncaught exception DivByZero at function div!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test exception sequence 8") {
    val prog = """
    {foo(a,b) = first([0,1,plus([],[]),div(a,b),34])}
    foo(245,0)
               """
    assertResult("Uncaught exception TypeMismatch at function plus!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test exceptions sequence 9") {
    val prog = """
    {foo(a,b) = minus([1],div(a,b)) }
    foo(12,0)
               """
    assertResult("Uncaught exception DivByZero at function div!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test exceptions sequence 10") {
    val prog = """
    {foo(a,b) = mult([1],div(a,b)) }
    foo(12,0)
               """
    assertResult("Uncaught exception DivByZero at function div!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test exceptions sequence 11") {
    val prog = """
    {foo(a,b) = div([1],div(a,b)) }
    foo(12,0)
               """
    assertResult("Uncaught exception DivByZero at function div!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test inc function") {
    val prog = """
    {}
    inc(inc(1))
               """
    assertResult("3") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test dec function") {
    val prog = """
    {}
    dec(dec(3))
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test inc function exception") {
    val prog = """
    {}
    inc(inc([1]))
               """
    assertResult("Uncaught exception TypeMismatch at function inc!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("Test dec function exception") {
    val prog = """
    {}
    dec(dec([2]))
               """
    assertResult("Uncaught exception TypeMismatch at function dec!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("bad program no condition") {
    val prog = """
    {length(x) = if badcond?(x,[]) then 0 else length(plus(1, length(rest(x))))}
    length([1,2,3,4,5,6,7,8])
               """
    assertResult("Interpretation failed! Condition not declared: badcond") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("bad program no function") {
    val prog = """
    {}
    test([])
               """
    assertResult("Interpretation failed! Function not declared: test") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("userfunction 1") {
    val prog = """
    {fak(x) = if eq?(x,0) then 1 else mult(x, fak(minus(x,1)))}
    fak(6)
               """
    assertResult("720") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("userfunction 2") {
    val prog = """
   {length(x) = if eq?(x,[]) then 0 else plus(1, len(rest(x)))}
   length([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20])
               """
    assertResult("20") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("infinite recursion") {
    val prog = """
   {foo(x) = foo(x)}
   foo(2)
               """
    assertResult("Stackoverflow! Maybe infinite recursion.") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("built in func redefinition") {
    val prog = """
   {plus(x,y) = x}
   plus(2,1)
               """
    assertResult("2") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("gt 1") {
    val prog = """
   {foo(x,y) = if gt?(x,y) then 0 else 1}
   foo(2,1)
               """
    assertResult("0") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("gt 2") {
    val prog = """
   {foo(x,y) = if gt?(y,x) then 0 else 1}
   foo(2,1)
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("len 1") {
    val prog = """
   {}
   len([1,2])
               """
    assertResult("2") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("len 2") {
    val prog = """
   {}
   len(2)
               """
    assertResult("Uncaught exception TypeMismatch at function len!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("reverse") {
    val prog = """
   {}
   reverse([1,2,3])
               """
    assertResult("[3,2,1]") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("big num") {
    val prog = """
   {
    foo(x) = if eq?(x,[]) then [] else build(bar(first(x)), foo(rest(x)));
    bar(x) = pow(x,mult(2,x));
    pow(x,y) = if eq?(y,0) then 1 else mult(x, pow(x, sub(y,1)))
   }
   foo([3,5,10])
               """
    assertResult("[729,9765625,7766279631452241920]") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("eq type mismatch") {
    val prog = """
   {
    func(x,y) = if eq?(x,y) then 1 else 0
   }
   func(0,[])
               """
    assertResult("Uncaught exception TypeMismatch at function eq!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("is0") {
    val prog = """
   {
    func(x) = if is0?(x) then 1 else 0
   }
   func(0)
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("is1") {
    val prog = """
   {
    func(x) = if is1?(x) then 1 else 0
   }
   func(1)
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("multilist build") {
    val prog = """
   {
   }
   build([1,1],[2,2])
               """
    assertResult("[[1,1],2,2]") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("mod") {
    val prog = """
   {
   }
   mod(3,2)
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("fakVal") {
    val prog = """
   {
   }
   fak(5)
               """
    assertResult("120") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("fak0") {
    val prog = """
   {
   }
   fak(0)
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("sqrt0") {
    val prog = """
   {
   }
   sqrt(0)
               """
    assertResult("0") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("sqrtVal") {
    val prog = """
   {
   }
   sqrt(4)
               """
    assertResult("2") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("sqrtVal2") {
    val prog = """
   {
   }
   sqrt(5)
               """
    assertResult("2") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("rand") {
    val prog = """
   {
   }
   rand(10)
               """
    val res = SWPInterpreter.evaluateProgram(prog).toInt
    assert(res <= 10 && res >= 0)
  }
  test("intConstants") {
    val prog = """
   {
   }
   plus(INTMIN, INTMAX)
               """
    assertResult("-1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("fewArgs") {
    val prog = """
   {
   }
   plus(INTMIN)
               """
    assertResult("Interpretation failed! Too few args for func plus. Only 1 provided!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("fewArgsCustom") {
    val prog = """
   {
   fun(x,y) = y
   }
   fun(1)
               """
    assertResult("Interpretation failed! Too few args for func fun. Only 1 provided!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("gcd") {
    val prog = """
   {
   }
   gcd(2016, 420)
               """
    assertResult("84") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("pow1") {
    val prog = """
   {
   }
   pow(3, 4)
               """
    assertResult("81") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("pow2") {
    val prog = """
   {
   }
   pow(15, 0)
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("pow3") {
    val prog = """
   {
   }
   pow(15, 1)
               """
    assertResult("15") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("atom1") {
    val prog = """
   {
   }
   if atom?(1) then 1 else 0
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("atom2") {
    val prog = """
   {
   }
   if atom?([1]) then 1 else 0
               """
    assertResult("0") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("atom3") {
    val prog = """
   {
   }
   if atom?(first([1,2])) then 1 else 0
               """
    assertResult("1") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("string1") {
    val prog = """
   {
   }
   "YES"
               """
    assertResult("\"YES\"") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("string2") {
    val prog = """
   {
   }
   if eq?("YES", "YES") then "YES" else "NO"
               """
    assertResult("\"YES\"") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("string3") {
    val prog = """
   {
   }
   if eq?("YES", 1) then "YES" else "NO"
               """
    assertResult("Uncaught exception TypeMismatch at function eq!") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("string4") {
    val prog = """
   {
   }
   len("TEST")
               """
    assertResult("4") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
  test("ne1") {
    val prog = """
   {
   }
   if ne?(1,-1) then "YES" else "NO"
               """
    assertResult("\"YES\"") {
      SWPInterpreter.evaluateProgram(prog)
    }
  }
}