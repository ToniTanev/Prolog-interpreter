
import scala.collection.mutable


case class Solver(program: Program, variablesToFind: List[String]) {


  var hasSolution: Boolean = false
  val resultSolutions: mutable.Queue[String] = mutable.Queue[String]()


  def solve(state: State, maxDepth: Int): Unit = {
    if(maxDepth > 0) {

      if (state.isSolving) {
        hasSolution = true          //we've found at least one solution
        resultSolutions.appendAll(state.getSolutions(variablesToFind)) //may fail appending, needs iterable
      }

      else if (!state.isBad) {

        if(state.queries.isEmpty)       //apply unification algorithm
          solve(state.applyUnificationAlgorithm, maxDepth - 1)

        else {
          val currQuery = state.queries.head

          if (program.isBuiltInSymbol(currQuery.identifier)) {
            val builtInFact = program.factForSymbol(currQuery.identifier)

            solve(State(state.queries.tail, Utilities.getAtomsComparing(currQuery, builtInFact) ::: state.constraints), maxDepth - 1)
          }

          else {

            if(program.rulesForAtom(currQuery).nonEmpty) {
              for {rule <- program.rulesForAtom(currQuery)} {
                val substitutedResultAtom = rule.resultAtom.substitute(Utilities.basicSubstitution(rule.resultAtom.getVariables))

                solve(State(rule.inputAtoms ::: state.queries.tail, Utilities.getAtomsComparing(currQuery, substitutedResultAtom) ::: state.constraints), maxDepth - 1)
              }
            }
            else {
              //no rules lead to this atom, stop process
            }

          }
        }
      }

      else {
        //state is bad, do nothing
      }
    }
    else {
      //max depth reached => do nothing, stop process
    }

  }

  def print(): Unit = {
    if (hasSolution) {
      println("Yes")
      println(resultSolutions)
    }
    else
      println("No")
  }

}