import net.thewalkingthread.exp.interpreter.SWPInterpreter
import org.scalatest.FunSuite

class SWPInterpreterTests extends FunSuite {

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

  test("Interpreter program with only build in functions") {
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
    assertResult("Uncaught exception DivByZero!"){
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





}