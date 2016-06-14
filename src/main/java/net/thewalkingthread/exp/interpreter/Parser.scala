package net.thewalkingthread.exp.interpreter

import scala.util.parsing.combinator._

class ExpParser extends JavaTokenParsers {

  //Nonterminale
  val program: Parser[Program] = funcDeclarations ~ expression ^^ {
    case fs ~ e => Program(fs,e)
  }

  private def funcDeclarations: Parser[List[FunctionDeclaration]] = "{" ~> repsep(funcDecl, ";") <~ "}"

  private def funcDecl: Parser[FunctionDeclaration] =
    identifier ~ "(" ~ repsep(identifier, ",") ~ ")" ~ "=" ~ expression ^^ {
      case n~_~xs~_~_~e => FunctionDeclaration(n,xs,e,xs.size)
    }

  private def expression: Parser[Expression] = cond | myThrow | tryCatch | function | int | string | list | variable

  private def predicate: Parser[Predicate] = identifier ~ "?" ~ "(" ~ repsep(expression, ",") <~ ")" ^^ {
    case i~_~_~es => Predicate(i,es)
  }

  //Terminale
  val int: Parser[ExpInt] = "(-?[1-9][0-9]*)|0".r ^^ { x => ExpInt(x.toInt)}
  val string : Parser[ExpString] = "\"" ~> identifier <~ "\"" ^^ { x => ExpString(x)}

  val identifier : Parser[String] = "[a-zA-Z][a-zA-Z0-9]*".r

  val variable: Parser[ExpVariable] = identifier ^^ {x => ExpVariable(x)}

  val list: Parser[ExpList] = ("[" ~> repsep(expression, ",") <~ "]") ^^ {
    case es => ExpList(es)
  }

  val exceptionId: Parser[String] = "[A-Z][A-Za-z]*".r

  val handler: Parser[Handler] = (exceptionId | "_") ~ ":" ~ expression ^^ {
    case handlerId~_~ex => Handler(handlerId, ex)
  }

  val tryCatch: Parser[ExpTryCatch] = "try" ~> (expression <~ "catch") ~ ("{" ~> repsep(handler, ";")) <~ "}" ^^ {
    case ex~handlerList => ExpTryCatch(ex, handlerList)
  }

  val function: Parser[ExpFunction] = (identifier <~ "(") ~ repsep(expression, ",") <~ ")" ^^ {
    case id~es => ExpFunction(id, es)
  }

  val myThrow: Parser[ExpThrow] = "throw" ~> exceptionId ^^ {
    x => ExpThrow(x)
  }

  val cond: Parser[ExpCond] = "if" ~> (predicate <~ "then") ~ (expression <~ "else") ~ expression ^^ {
    case p~e1~e2 => ExpCond(p,e1,e2)
  }
}

object ParseProgram extends ExpParser {
  def parse(s: String): ParseResult[Program] = {
    parseAll(program, s)
  }
}