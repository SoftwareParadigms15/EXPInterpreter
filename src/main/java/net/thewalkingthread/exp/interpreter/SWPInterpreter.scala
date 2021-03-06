package net.thewalkingthread.exp.interpreter

object SWPInterpreter {

  def main(args: Array[String]) {
    val Array(file) = args
    val source = scala.io.Source.fromFile(file)
    val fileString = source.mkString
    source.close()

    val result = evaluateProgram(fileString)

    Console.println("----------")
    Console.println(result)
    Console.println("----------")
  }



  def evaluateProgram(program:String):String = {
    try {
      val parserResult = ParseProgram.parse(program)
      if (parserResult.successful) {
        val Program(funcs, exp) = parserResult.get
        val result = Interpreter.interpret(funcs.map(x=>(x.name, x)).toMap, Map(), exp)
        PrettyPrinter.print(result)
      } else  {
        val errorMsg = PrettyPrinter.formatError(parserResult.toString)
        throw new InterpreterFailedException(errorMsg)
      }
    } catch {
      case InterpreterFailedException(msg) => "Interpretation failed! "+msg
      case ExpInternalException(msg, funcName) => "Uncaught exception %s at function %s!".format(msg, funcName)
      case e: RuntimeException => e.getMessage
      case e: Exception => e.getClass.getName + ": " + e.getMessage
      case e: StackOverflowError => "Stackoverflow! Maybe infinite recursion."
    }
  }


  def checkProgramGrammer(program:String):Boolean = {
    ParseProgram.parse(program).successful
  }
  
}
