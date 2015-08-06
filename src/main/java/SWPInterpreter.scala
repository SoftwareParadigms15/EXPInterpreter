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
      val Program(funcs, exp) = ParseProgram.parse(program).get
      val result = Interpreter.interpret(funcs.map(x=>(x.name, x)).toMap, Map(), exp)
      PrettyPrinter.print(result)
    } catch {
      case InterpreterFailedException(msg) => "Interpretation failed! "+msg
      case e: Exception => e.getMessage
      case e: StackOverflowError => "Stackoverflow! Maybe infinite recursion."
    }
  }


  def checkProgramGrammer(program:String):Boolean = {
    ParseProgram.parse(program).successful
  }
  
}