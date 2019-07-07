

case class State(queries: List[Atom], constraints: List[Atom]) {

  def constraintsContainDuplicateVariables: Boolean = {
    val variablesInEquations = constraints.filter(_.isSolving).map(_.terms.head)

    val duplicateVariables = variablesInEquations.diff(variablesInEquations.distinct).distinct

    duplicateVariables.nonEmpty
  }

  def isDuplicateVariable(x: String): Boolean = {
    val variablesInEquations = constraints.filter(_.isSolving).map(_.terms.head)

    variablesInEquations.count(_ == x) > 1
  }

  def deleteConstraint( constraint: Atom ): State = {
    State( queries, constraints.filter(_ != constraint) )
  }

  def addConstraint( constraint: Atom ): State = {
    State ( queries, constraint :: constraints )
  }

  def isBad: Boolean = {                                                  //dead end state, it cannot be continued
    constraints.exists(_.isBad) || constraintsContainDuplicateVariables   //contains a bad equation or has duplicate variables
  }

  def isSolving: Boolean = {
    queries.isEmpty && (constraints.isEmpty || (constraints.forall(_.isSolving) && !constraintsContainDuplicateVariables) )
  }

  def findSolutionForOneVariable(variable: String): String = {
    if( isSolving )       //we're sure the variable is present in the equations only once; we assume the state is solving, but check anyway
    {
      val constraintContainingTheVariable = constraints.filter(_.terms.head == variable).head
      val secondTerm                      = constraintContainingTheVariable.terms.tail.head         //the equation is: variable = secondTerm; so check secondTerm

      if ( Parser.isVariable( secondTerm ) )
        findSolutionForOneVariable( secondTerm )
      else    //not a variable => directly return it as a result solution for the variable
        secondTerm
    }
    else    //should not get here
      ""
  }

  def getSolutions(variablesToFind: List[String]): List[String] = {        //only if it is solving
    if (isSolving) { //we assume the state is solving, but check anyway
      if (variablesToFind.nonEmpty) {
        findSolutionForOneVariable(variablesToFind.head) :: getSolutions(variablesToFind.tail)
      }
      else{
        List.empty
      }
    }
    else    //should not get here
      List.empty
  }

  def getVariables: List[String] = queries.flatMap(_.getVariables) ::: constraints.flatMap(_.getVariables)

  def containsVariable(variable: String): Boolean = getVariables.contains(variable)

  def applyUnificationAlgorithm: State = {    //we assume all constraints are equations (algorithm without occurs check); also assume queries are empty

    if(!isSolving && !isBad) {
      val filteredConstraints = constraints.filter { constraint => {                            //filter out the trivial cases
        val first = constraint.terms.head
        val second = constraint.terms.tail.head
        !(Parser.isVariable(first) && Parser.isVariable(second) && (first == second)) &&      //second solving transform
          !(Parser.isConstant(first) && Parser.isConstant(second) && (first == second))         //third trivial solving transform
      }
      }

      val filteredState = State(List.empty, filteredConstraints)

      var substitutions = Map[String, String]()

      val newConstraints = filteredConstraints.flatMap { constraint => {
        val first = constraint.terms.head         //first term (arg)
        val second = constraint.terms.tail.head   //second term (arg)

        if (Parser.isTerm(first) && !Parser.isVariable(first) && Parser.isVariable(second))                        //first solving transform
          List(Atom('='.toString, List(second, first)))

        else if (Parser.isAtom(first) && Parser.isAtom(second))                       //third non trivial solving transform
          Utilities.getAtomsComparing(AtomOps(first).atom, AtomOps(second).atom)

        else if (Parser.isVariable(first) && Parser.isTerm(second) && filteredState.deleteConstraint(constraint).containsVariable(first) ) {                //fourth solving transform => mark variable for substitution and mark current equation not to be substituted
          substitutions += (first -> second)
          List(Atom(constraint.identifier, constraint.terms, substitutable = false))
        }

        else
          List(constraint)

      }
      }

      State(List.empty, newConstraints).substitute(substitutions).applyUnificationAlgorithm     //apply unification algorithm while we can (while we haven't reached solving or bad state)

    }

    else         //just do nothing
      this

  }

  def substitute(implicit variablesSubstitution: Map[String, String]): State = {
    State( queries.map(_.substitute(variablesSubstitution)), constraints.map(_.substitute(variablesSubstitution)) )
  }

}

