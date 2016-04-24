package net.thewalkingthread.exp.interpreter

import java.math.BigInteger

import scala.util.Random

case class ExpInternalException(handleWith: String, funcName: String) extends Exception
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
    case _ => throw ExpInternalException("TypeMismatch","plus")
  }

  def builtinMinus(a: Value, b:Value) = (a, b) match {
    case (ValInt(x), ValInt(y)) => ValInt(x - y)
    case _ => throw ExpInternalException("TypeMismatch","minus")
  }

  def builtinMult(a: Value, b:Value) = (a, b) match {
    case (ValInt(x), ValInt(y)) => ValInt(x * y)
    case _ => throw ExpInternalException("TypeMismatch","mult")
  }

  def builtinDiv(a: Value, b:Value) = (a, b) match {
    case (ValInt(_), ValInt(0)) => throw ExpInternalException("DivByZero", "div")
    case (ValInt(x), ValInt(y)) => ValInt(x / y)
    case _ => throw ExpInternalException("TypeMismatch","div")
  }

  def builtinFirst(args: Value) = args match {
    case ValList(Nil) => throw ExpInternalException("EmptyList", "first")
    case ValList(v) => v.head
    case ValString(v) => ValString(v.charAt(0).toString)
    case _ => throw ExpInternalException("TypeMismatch", "first")
  }

  def builtinRest(args: Value) = args match {
    case ValList(Nil) => throw ExpInternalException("EmptyList", "rest")
    case ValList(v::vs) => ValList(vs)
    case ValString(v) => ValString(v.substring(1))
    case _ => throw ExpInternalException("TypeMismatch", "rest")
  }

  def builtinBuild(a: Value, b: Value) = (a, b) match {
    case (ValInt(iv), ValList(lv)) => ValList(a::lv)
    case (ValList(iv), ValList(lv)) => ValList(a::lv)
    case (ValString(iv), ValString(lv)) => ValString(iv+lv)
    case _ => throw ExpInternalException("TypeMismatch", "build")
  }

  def builtinInc(a: Value) = a match {
    case ValInt(x) => ValInt(x+1)
    case _ => throw ExpInternalException("TypeMismatch", "inc")
  }

  def builtinDec(a: Value) = a match {
    case ValInt(x) => ValInt(x-1)
    case _ => throw ExpInternalException("TypeMismatch", "dec")
  }

  def builtinLen(a: Value) = a match {
    case ValList(x) => ValInt(x.length)
    case ValString(v) => ValInt(v.length)
    case _ => throw ExpInternalException("TypeMismatch", "len")
  }

  def builtinReverse(a: Value) = a match {
    case ValList(x) => ValList(x.reverse)
    case ValString(v) => ValString(v.reverse)
    case _ => throw ExpInternalException("TypeMismatch", "reverse")
  }

  def builtinMod(a:Value, b:Value) = (a,b) match {
    case (ValInt(x), ValInt(y)) => ValInt(x % y)
    case _ => throw ExpInternalException("TypeMismatch", "mod")
  }

  def builtinFak(a: Value) = {
    def factorial(x: Long, res: Long): Long = {
      if (x == 0 || x == 1) res
      else factorial(x - 1, res * x)
    }
    a match {
      case ValInt(x) if x < 21 => ValInt(factorial(x, 1))
      case ValInt(_) => throw ExpInternalException("ValueTooBig", "fak")
      case _ => throw ExpInternalException("TypeMismatch", "fak")
    }
  }

  def buitinSqrt(a: Value) = a match {
    case ValInt(x) => ValInt(Math.sqrt(x).floor.toLong)
    case _ => throw ExpInternalException("TypeMismatch", "sqrt")
  }

  def builtinAbs(a: Value) = a match {
    case ValInt(x) => ValInt(Math.abs(x))
    case _ => throw ExpInternalException("TypeMismatch", "abs")
  }

  def builtinRand(a: Value) = a match {
    case ValInt(x) if x < Int.MaxValue => ValInt(Random.nextInt(x.toInt))
    case ValInt(_) => throw ExpInternalException("ValueTooBig", "rand")
    case _ => throw ExpInternalException("TypeMismatch", "rand")
  }

  def builtinGcd(a:Value, b:Value) = (a,b) match {
    case (ValInt(x), ValInt(y)) if x < Long.MaxValue && y < Long.MaxValue => ValInt(BigInteger.valueOf(x).gcd(BigInteger.valueOf(y)).longValue())
    case (ValInt(_), ValInt(_)) => throw ExpInternalException("ValueTooBig", "gcd")
    case _ => throw ExpInternalException("TypeMismatch", "gcd")
  }

  def builtinPow(a:Value, b:Value) = (a,b) match {
    case (ValInt(_), ValInt(0)) => ValInt(1)
    case (ValInt(x), ValInt(1)) => ValInt(x)
    case (ValInt(x), ValInt(y)) if x < Long.MaxValue && y < Long.MaxValue => ValInt(BigInteger.valueOf(x).pow(y.toInt).longValue())
    case (ValInt(_), ValInt(_)) => throw ExpInternalException("ValueTooBig", "pow")
    case _ => throw ExpInternalException("TypeMismatch", "pow")
  }

  def predEq(a: Value, b: Value) = (a,b) match {
    case (ValInt(x), ValInt(y)) => x == y
    case (ValList(x), ValList(y)) => x == y
    case (ValString(x), ValString(y)) => x == y
    case (_,_) => throw ExpInternalException("TypeMismatch", "eq")
  }

  def predLt(a: Value, b: Value) = (a,b) match {
    case (ValInt(v1), ValInt(v2)) => v1 < v2
    case _ => throw ExpInternalException("TypeMismatch", "lt")
  }

  def predGt(a: Value, b: Value) = (a,b) match {
    case (ValInt(v1), ValInt(v2)) => v1 > v2
    case _ => throw ExpInternalException("TypeMismatch", "gt")
  }

  def predIs0(a: Value) = a match {
    case ValInt(x) => x == 0
    case _ => throw ExpInternalException("TypeMismatch", "is0")
  }

  def predIs1(a: Value) = a match {
    case ValInt(x) => x == 1
    case _ => throw ExpInternalException("TypeMismatch", "is1")
  }

  def predAtom(a: Value) = a match {
    case ValInt(_) => true
    case ValString(_) => true
    case ValList(_) => false
    case ValUncaughtException(_) => false
  }

  def predNe(a: Value, b: Value) = (a,b) match {
    case (ValInt(x), ValInt(y)) => x == (y * -1)
    case (_,_) => throw ExpInternalException("TypeMismatch", "ne")
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
        case ExpString(v) => ValString(v)

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
            case ExpInternalException(handleWith, funcName) => {
              for (h <- handlers) {
                if (h.exceptionId == handleWith || h.exceptionId == "_") {
                  return interpret(functionEnvironment, variableEnvironment, h.exp)
                }
              }
              throw ExpInternalException(handleWith, funcName)
            }
          }
        }

        case ExpThrow(exceptionId) => throw ExpInternalException(exceptionId, "userfunc")

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

    case ExpCond(Predicate("atom", params), e1, e2) => {
      if (predAtom(interpret(functionEnvironment, variableEnvironment, params.head)))
        interpret(functionEnvironment, variableEnvironment, e1)
      else
        interpret(functionEnvironment, variableEnvironment, e2)
    }

    case ExpCond(Predicate("ne", params), e1, e2) => {
      if (predNe(interpret(functionEnvironment, variableEnvironment, params.head), interpret(functionEnvironment, variableEnvironment,
        params(1))))
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

    case ExpFunction("pow", args: List[Expression]) => builtinPow(interpret(functionEnvironment, variableEnvironment, args.head),
      interpret(functionEnvironment, variableEnvironment, args(1)))

    case ExpFunction(funcIdentifier, _) => throw InterpreterFailedException("Function not declared: " + funcIdentifier)
  }
}