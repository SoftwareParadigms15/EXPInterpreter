package net.thewalkingthread.exp.interpreter

case class Program(functionEnvironment: List[FunctionDeclaration], main:Expression)

case class FunctionDeclaration(name:String, params: List[String], body: Expression, variableCount: Int)

//expressions
sealed abstract class Expression
case class ExpVariable(v:String) extends  Expression
case class ExpInt(v: Long) extends Expression
case class ExpList(v: List[Expression]) extends Expression
case class ExpString(v: String) extends Expression
case class ExpCond(p: Predicate, e1: Expression, e2:Expression)  extends  Expression
case class ExpTryCatch(tryExp: Expression, handlerList: List[Handler]) extends Expression
case class ExpFunction(id:String, ex:List[Expression]) extends Expression
case class ExpException(id:String) extends Expression
case class ExpThrow(exceptionId: String) extends Expression


case class Handler(exceptionId: String, exp: Expression)

case class Predicate(name: String, params: List[Expression])

//values
sealed abstract class Value
case class ValInt(v: Long) extends Value
case class ValList(v: List[Value]) extends Value
case class ValString(v: String) extends Value
case class ValUncaughtException(v: String) extends Value

object PrettyPrinter {
  def print(value: Value):String = value match {
    case ValInt(v) => v.toString
    case ValString(v) => "\"" + v + "\""
    case ValList(v)=> {
      var res = "["
      for (x <- v) {
        res += print(x) + ","
      }
      res.subSequence(0, res.length - 1) + "]"
    }
    case ValUncaughtException(v) => "Uncaught exception %s!".format(v)
  }

  def formatError(e: String): String = e.replace("\n", "").replaceAll(".*failure:(.+?)found.*", "$1 found").trim
}
