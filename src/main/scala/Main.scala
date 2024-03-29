
import java.nio.file.{Files, Paths}

import scala.io.StdIn


object Main extends App {

  var shouldContinue: Boolean = true
  var programFile: String     = ""
  var program: Program        = Program(List.empty, List.empty)

  while(shouldContinue){
    val line = StdIn.readLine()

    if(Utilities.loadProgramFrom(line)) {
      programFile = Utilities.getProgramFileFrom(line)

      if (Files.exists(Paths.get(programFile))) {
        program = Parser.parse(programFile)

        println("loaded " + programFile)
      }
      else{
        println("file not found")
      }

    }

    else if(Parser.isListOfAtoms(line)){
      val queries   = Parser.splitToListOfAtoms(line).map {AtomOps(_).atom}
      val variables = Utilities.getVariables(queries)
      val solver    = Solver(program, variables)

      solver.solve(State(queries, List.empty), 10 /* max depth */)
      solver.printSolutions()
    }

    else if(line == "exit" || line == "quit" || line == "q" || line == "stop")
      shouldContinue  = false

    else{
      //do nothing, continue
    }

  }
}
