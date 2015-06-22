case class Program(functionEnvironment: List[FunctionDeclaration], main:Expression)

case class FunctionDeclaration(name:String, params: List[String], body: Expression)


sealed abstract class Expression
case class ExpVariable(v:String) extends  Expression
case class ExpInt(v: Int) extends Expression
case class ExpList(v: List[Expression]) extends Expression
case class ExpCond(p: Predicate, e1: Expression, e2:Expression)  extends  Expression
case class ExpTryCatch(tryExp: Expression, handlerList: List[Handler]) extends Expression
case class ExpFunction(id:String, ex:List[Expression]) extends Expression
case class ExpException(id:String) extends Expression
case class ExpThrow(exceptionId: String) extends Expression



//TODO Add case classes to represent the Expression AST
case class Handler(exceptionId: String, exp: Expression)

case class Predicate(name: String, params: List[Expression])


sealed abstract class Value
case class ValInt(v: Int) extends Value
case class ValList(v: List[Value]) extends Value
case class ValUncaughtException(v: String) extends Value

//TODO Add case classes to represent the Value AST


object PrettyPrinter {
  def print(value: Value):String = value match {
    case ValInt(v) => v.toString
    case ValList(v)=> {
      var res = "["
      for (x <- v) {
        res += print(x) + ","
      }
      res.subSequence(0, res.length - 1) + "]"
    }
    case ValUncaughtException(v) => "Uncaught exception %s!".format(v)
  }
  //TODO This method should produce formated output for the output values

  //An Integer is printed as the Integer itself e.g. 42

  //A List is printed as the printed Elements separated by a colon and enclosed with square brackets e.g. [42,[],1]
  //  Scala's mkString function might be helpful to do this.

  //A Exception Value is printed as the String "Uncaught exception $ex!", where $ex is replaced by the name of the exception
  //  e.g. Uncaught exception DivByZero!
}
