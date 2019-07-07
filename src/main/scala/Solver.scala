
import scala.collection.mutable


case class Solver(program: Program, variablesToFind: List[String]) {


  var hasSolution: Boolean = false
  val resultSolutions: mutable.Queue[Solution] = mutable.Queue.empty


  def solve(state: State, maxDepth: Int): Unit = {
    if(maxDepth > 0) {

      if (state.isSolving) {
        hasSolution = true          //we've found at least one solution
        resultSolutions.append(Solution(variablesToFind, state.getSolutions(variablesToFind)))
      }

      else if (!state.isBad) {

        if(state.queries.isEmpty)       //apply unification algorithm
          solve(state.applyUnificationAlgorithm, maxDepth - 1)

        else {
          val currQuery = state.queries.head

          if (program.isBuiltInSymbol(currQuery.identifier)) {
            for { fact <- program.factsForSymbol(currQuery.identifier) } {

              solve(State(state.queries.tail, Utilities.getAtomsComparing(currQuery, fact) ::: state.constraints), maxDepth - 1)
            }
          }

          else {

            if(program.rulesForAtom(currQuery).nonEmpty) {
              for {rule <- program.rulesForAtom(currQuery)} {
                val substitutedRule = rule.substitute(Utilities.basicSubstitution(rule.getVariables))

                solve(State(substitutedRule.inputAtoms ::: state.queries.tail, Utilities.getAtomsComparing(currQuery, substitutedRule.resultAtom) ::: state.constraints), maxDepth - 1)
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

  def printSolutions(): Unit = {
    if (hasSolution) {
      println("Yes")
      for {solution <- resultSolutions} solution.print()
    }
    else
      println("No")
  }


}