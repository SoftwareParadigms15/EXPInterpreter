package net.thewalkingthread.exp.interpreter

import java.math.BigInteger

import scala.util.Random

case class ExpInternalException(handleWith: String) extends Exception
case class InterpreterFailedException(msg: String) extends Exception

object Interpreter {

  //Type aliases to make the signatures more readable
  type FunctionName = String
  type VariableName = String

  //map for constants
  val constants = Map[String, Value](
    "INTMAX" -> ValInt(Int.MaxValue),
    "INTMIN" -> ValInt(Int.MinValue)
  )

  def builtinPlus(a: Value, b:Value) = (a, b) match {
    case (ValInt(x), ValInt(y)) => ValInt(x + y)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinMinus(a: Value, b:Value) = (a, b) match {
    case (ValInt(x), ValInt(y)) => ValInt(x - y)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinMult(a: Value, b:Value) = (a, b) match {
    case (ValInt(x), ValInt(y)) => ValInt(x * y)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinDiv(a: Value, b:Value) = (a, b) match {
    case (ValInt(_), ValInt(0)) => throw ExpInternalException("DivByZero")
    case (ValInt(x), ValInt(y)) => ValInt(x / y)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinFirst(args: Value) = args match {
    case ValList(Nil) => throw ExpInternalException("EmptyList")
    case ValList(v) => v.head
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinRest(args: Value) = args match {
    case ValList(Nil) => throw ExpInternalException("EmptyList")
    case ValList(v::vs) => ValList(vs)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinBuild(a: Value, b: Value) = (a, b) match {
    case (ValInt(iv), ValList(lv)) => ValList(a::lv)
    case (ValList(iv), ValList(lv)) => ValList(a::lv)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinInc(a: Value) = a match {
    case ValInt(x) => ValInt(x+1)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinDec(a: Value) = a match {
    case ValInt(x) => ValInt(x-1)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinLen(a: Value) = a match {
    case ValList(x) => ValInt(x.length)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinReverse(a: Value) = a match {
    case ValList(x) => ValList(x.reverse)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinMod(a:Value, b:Value) = (a,b) match {
    case (ValInt(x), ValInt(y)) => ValInt(x % y)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinFak(a: Value) = {
    def factorial(x: Long, res: Long): Long = {
      if (x == 0 || x == 1) res
      else factorial(x - 1, res * x)
    }
    a match {
      case ValInt(x) => ValInt(factorial(x, 1))
      case _ => throw ExpInternalException("TypeMismatch")
    }
  }

  def buitinSqrt(a: Value) = a match {
    case ValInt(x) => ValInt(Math.sqrt(x).floor.toLong)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinAbs(a: Value) = a match {
    case ValInt(x) => ValInt(Math.abs(x))
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinRand(a: Value) = a match {
    case ValInt(x) if x < Int.MaxValue => ValInt(Random.nextInt(x.toInt))
    case ValInt(_) => throw ExpInternalException("Value too big in function %s" format "rand")
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinGcd(a:Value, b:Value) = (a,b) match {
    case (ValInt(x), ValInt(y)) => {
      try ValInt(BigInteger.valueOf(x).gcd(BigInteger.valueOf(y)).longValueExact())
      catch {
        case e: ArithmeticException => throw ExpInternalException("Value too big in function %s" format "gcd")
      }
    }
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def predEq(a: Value, b: Value) = (a,b) match {
    case (ValInt(x), ValInt(y)) => x == y
    case (ValList(x), ValList(y)) => x == y
    case (_,_) => throw ExpInternalException("TypeMismatch")
  }

  def predLt(a: Value, b: Value) = (a,b) match {
    case (ValInt(v1), ValInt(v2)) => v1 < v2
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def predGt(a: Value, b: Value) = (a,b) match {
    case (ValInt(v1), ValInt(v2)) => v1 > v2
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def predIs0(a: Value) = a match {
    case ValInt(x) => x == 0
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def predIs1(a: Value) = a match {
    case ValInt(x) => x == 1
    case _ => throw ExpInternalException("TypeMismatch")
  }


  def interpret(functionEnvironment: Map[FunctionName, FunctionDeclaration],
                variableEnvironment: Map[VariableName, Value],
                expression: Expression): Value = {
    try {
      expression match {
        case ExpVariable(name) => variableEnvironment get name match {
          case Some(n) => n
          case None => constants get name match {
            case Some(n) => n
            case None => throw InterpreterFailedException("Variable not declared: " + name)
          }
        }

        case ExpInt(v) => ValInt(v)

        case ExpList(v) => ValList(v.map(ex => interpret(functionEnvironment, variableEnvironment, ex)))

        case ExpFunction(funcIdentifier, args: List[Expression]) => {
          val fnDeclaration = functionEnvironment get funcIdentifier
          fnDeclaration match {
            case Some(FunctionDeclaration(_, params, body)) => {
              val interpretedArgs = args.map(x => interpret(functionEnvironment, variableEnvironment, x))
              val newEnv = params.zip(interpretedArgs).toMap
              interpret(functionEnvironment, newEnv, body)
            }
            case None => interpret_builtin(functionEnvironment, variableEnvironment, expression.asInstanceOf[ExpFunction])
          }
        }

        case ExpCond(_, _, _) => interpret_cond(functionEnvironment, variableEnvironment, expression.asInstanceOf[ExpCond])

        case ExpTryCatch(tryExpression: Expression, handlers: List[Handler]) => {
          try {
            interpret(functionEnvironment, variableEnvironment, tryExpression)
          } catch {
            case ExpInternalException(handleWith) => {
              for (h <- handlers) {
                if (h.exceptionId == handleWith || h.exceptionId == "_") {
                  return interpret(functionEnvironment, variableEnvironment, h.exp)
                }
              }
              throw ExpInternalException(handleWith)
            }
          }
        }

        case ExpThrow(exceptionId) => throw ExpInternalException(exceptionId)

        case _ => throw new InterpreterFailedException("Unknown expression")
      }
    } catch {
      //case ExpInternalException(exceptId) => ValUncaughtException(exceptId)
      case e: IndexOutOfBoundsException => {
        expression match {
          case ExpFunction(name, args) => throw new InterpreterFailedException("Too few args for func %s. Only %s provided!".format(name,
            args
            .size))
          case _ => throw e
        }
      }
    }
  }

  def interpret_cond(functionEnvironment: Map[FunctionName, FunctionDeclaration],
                     variableEnvironment: Map[VariableName, Value],
                     expression: ExpCond): Value = expression match {
    case ExpCond(Predicate("eq", params), e1, e2) => {
      if (predEq(interpret(functionEnvironment, variableEnvironment, params.head), interpret(functionEnvironment, variableEnvironment, params(1))))
        interpret(functionEnvironment, variableEnvironment, e1)
      else
        interpret(functionEnvironment, variableEnvironment, e2)
    }

    case ExpCond(Predicate("lt", params), e1, e2) => {
      if (predLt(interpret(functionEnvironment, variableEnvironment, params.head), interpret(functionEnvironment, variableEnvironment, params(1))))
        interpret(functionEnvironment, variableEnvironment, e1)
      else
        interpret(functionEnvironment, variableEnvironment, e2)
    }

    case ExpCond(Predicate("gt", params), e1, e2) => {
      if (predGt(interpret(functionEnvironment, variableEnvironment, params.head), interpret(functionEnvironment, variableEnvironment, params(1))))
        interpret(functionEnvironment, variableEnvironment, e1)
      else
        interpret(functionEnvironment, variableEnvironment, e2)
    }

    case ExpCond(Predicate("is0", params), e1, e2) => {
      if (predIs0(interpret(functionEnvironment, variableEnvironment, params.head)))
        interpret(functionEnvironment, variableEnvironment, e1)
      else
        interpret(functionEnvironment, variableEnvironment, e2)
    }

    case ExpCond(Predicate("is1", params), e1, e2) => {
      if (predIs1(interpret(functionEnvironment, variableEnvironment, params.head)))
        interpret(functionEnvironment, variableEnvironment, e1)
      else
        interpret(functionEnvironment, variableEnvironment, e2)
    }

    case ExpCond(Predicate(func, _), _, _) => throw new InterpreterFailedException("Condition not declared: " + func)
  }

  def interpret_builtin(functionEnvironment: Map[FunctionName, FunctionDeclaration],
                        variableEnvironment: Map[VariableName, Value],
                        expression: ExpFunction): Value = expression match {

    case ExpFunction("plus", args: List[Expression]) => builtinPlus(interpret(functionEnvironment, variableEnvironment, args.head),
      interpret(functionEnvironment, variableEnvironment, args(1)))
    case ExpFunction("add", args: List[Expression]) => builtinPlus(interpret(functionEnvironment, variableEnvironment, args.head),
      interpret(functionEnvironment, variableEnvironment, args(1)))

    case ExpFunction("minus", args: List[Expression]) => builtinMinus(interpret(functionEnvironment, variableEnvironment, args.head),
      interpret(functionEnvironment, variableEnvironment, args(1)))
    case ExpFunction("sub", args: List[Expression]) => builtinMinus(interpret(functionEnvironment, variableEnvironment, args.head),
      interpret(functionEnvironment, variableEnvironment, args(1)))

    case ExpFunction("mult", args: List[Expression]) => builtinMult(interpret(functionEnvironment, variableEnvironment, args.head),
      interpret(functionEnvironment, variableEnvironment, args(1)))

    case ExpFunction("div", args: List[Expression]) => builtinDiv(interpret(functionEnvironment, variableEnvironment, args.head),
      interpret(functionEnvironment, variableEnvironment, args(1)))

    case ExpFunction("first", args: List[Expression]) => builtinFirst(interpret(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("rest", args: List[Expression]) => builtinRest(interpret(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("build", args: List[Expression]) => builtinBuild(interpret(functionEnvironment, variableEnvironment, args.head),
      interpret(functionEnvironment, variableEnvironment, args(1)))

    case ExpFunction("inc", args: List[Expression]) => builtinInc(interpret(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("dec", args: List[Expression]) => builtinDec(interpret(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("len", args: List[Expression]) => builtinLen(interpret(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("reverse", args: List[Expression]) => builtinReverse(interpret(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("mod", args: List[Expression]) => builtinMod(interpret(functionEnvironment, variableEnvironment, args.head),
      interpret(functionEnvironment, variableEnvironment, args(1)))

    case ExpFunction("fak", args: List[Expression]) => builtinFak(interpret(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("sqrt", args: List[Expression]) => buitinSqrt(interpret(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("abs", args: List[Expression]) => builtinAbs(interpret(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("rand", args: List[Expression]) => builtinRand(interpret(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("gcd", args: List[Expression]) => builtinGcd(interpret(functionEnvironment, variableEnvironment, args.head),
      interpret(functionEnvironment, variableEnvironment, args(1)))

    case ExpFunction(funcIdentifier, _) => throw InterpreterFailedException("Function not declared: " + funcIdentifier)
  }
}