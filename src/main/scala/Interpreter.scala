case class ExpInternalException(val handleWith: String) extends Exception

object Interpreter {

  //Type aliases to make the signatures more readable
  type FunctionName = String
  type VariableName = String

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
    case _ => println((a, b)); throw ExpInternalException("TypeMismatch")
  }

  def builtinDiv(a: Value, b:Value) = (a, b) match {
    case (ValInt(_), ValInt(0)) => throw ExpInternalException("DivByZero")
    case (ValInt(x), ValInt(y)) => ValInt(x / y)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinFirst(args: Value) = args match {
    case ValList(Nil) => throw ExpInternalException("EmptyList")
    case ValList(v) => v(0)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinRest(args: Value) = args match {
    case ValList(Nil) => throw ExpInternalException("EmptyList")
    case ValList(v::vs) => ValList(vs)
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def builtinBuild(a: Value, b: Value) = (a, b) match {
    case (ValInt(iv), ValList(lv)) => ValList(ValInt(iv)::lv)
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

  def predEq(a: Value, b: Value) = a == b

  def predLt(a: Value, b: Value) = (a,b) match {
    case (ValInt(v1), ValInt(v2)) => v1 < v2
    case _ => throw ExpInternalException("TypeMismatch")
  }

  def interpret(functionEnvironment: Map[FunctionName, FunctionDeclaration],
                variableEnvironment: Map[VariableName, Value],
                expression: Expression): Value = {
    try {
      interpret2(functionEnvironment, variableEnvironment, expression)
    } catch {
      case ExpInternalException(exceptId) => ValUncaughtException(exceptId)
    }

  }

  def interpret2(functionEnvironment: Map[FunctionName, FunctionDeclaration],
                variableEnvironment: Map[VariableName, Value],
                expression: Expression): Value = expression match {

    case ExpVariable(name) => variableEnvironment.get(name) match {
      case Some(n) => n
      case _ => ???
    }

    case ExpInt(v) => ValInt(v)

    case ExpList(v) => ValList(v.map(ex => interpret2(functionEnvironment, variableEnvironment, ex)))

    case ExpFunction("plus", args: List[Expression]) => builtinPlus(interpret2(functionEnvironment, variableEnvironment, args(0)),
                                                                    interpret2(functionEnvironment, variableEnvironment, args(1)))
    case ExpFunction("minus", args: List[Expression]) => builtinMinus(interpret2(functionEnvironment, variableEnvironment, args(0)),
                                                                      interpret2(functionEnvironment, variableEnvironment, args(1)))
    case ExpFunction("mult", args: List[Expression]) => builtinMult(interpret2(functionEnvironment, variableEnvironment, args(0)),
                                                                    interpret2(functionEnvironment, variableEnvironment, args(1)))
    case ExpFunction("div", args: List[Expression]) => builtinDiv(interpret2(functionEnvironment, variableEnvironment, args(0)),
                                                                  interpret2(functionEnvironment, variableEnvironment, args(1)))
    case ExpFunction("first", args: List[Expression]) => builtinFirst(interpret2(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("rest", args: List[Expression]) => builtinRest(interpret2(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("build", args: List[Expression]) => builtinBuild(interpret2(functionEnvironment, variableEnvironment, args(0)),
                                                                      interpret2(functionEnvironment, variableEnvironment, args(1)))

    case ExpFunction("inc", args: List[Expression]) => builtinInc(interpret2(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction("dec", args: List[Expression]) => builtinDec(interpret2(functionEnvironment, variableEnvironment, args.head))

    case ExpFunction(funcIdentifier, args: List[Expression]) => {
      val fnDeclaration = functionEnvironment get funcIdentifier
      fnDeclaration match {
        case Some(FunctionDeclaration(_, params, body)) => {
          val interpretedArgs = args.map(x => interpret2(functionEnvironment, variableEnvironment, x))
          val newEnv = params.zip(interpretedArgs).toMap
          interpret2(functionEnvironment, newEnv, body)
        }
        case _ => ???
      }
    }

    case ExpCond(Predicate("eq", params),e1,e2) => {
      if (predEq(interpret2(functionEnvironment, variableEnvironment, params(0)), interpret2(functionEnvironment, variableEnvironment, params(1))))
        interpret2(functionEnvironment, variableEnvironment, e1)
      else
        interpret2(functionEnvironment, variableEnvironment, e2)
    }

    case ExpCond(Predicate("lt", params),e1,e2) => {
      if (predLt(interpret2(functionEnvironment, variableEnvironment, params(0)), interpret2(functionEnvironment, variableEnvironment, params(1))))
        interpret2(functionEnvironment, variableEnvironment, e1)
      else
        interpret2(functionEnvironment, variableEnvironment, e2)
    }

    case ExpTryCatch(tryExpression: Expression, handlers: List[Handler]) => {
      try {
        interpret2(functionEnvironment, variableEnvironment, tryExpression)
      } catch {
        case ExpInternalException(handleWith) => {
          for (h <- handlers) {
            if (h.exceptionId == handleWith || h.exceptionId == "_") {
              return interpret2(functionEnvironment, variableEnvironment, h.exp)
            }
          }
          throw ExpInternalException(handleWith)
        }
      }
    }

    case ExpThrow(exceptionId) => throw ExpInternalException(exceptionId)



  }

  //TODO
  //This function should evaluate the given expression, using the functionEnvironment(delta)
  //and the variableEnvironment(omega). 
  //
  //It might be helpful to define helper functions to evaluate different expressions,
  //like buildins or user defined functions.


}
