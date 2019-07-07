
import scala.collection.mutable
import scala.io.Source



case class Program(facts: List[Atom], rules: List[Rule]) {
  def builtInAtomSymbols: List[String] = '='.toString :: ( for( fact <- facts ) yield fact.identifier )       //including "="

  def isBuiltInSymbol(symbol: String): Boolean = builtInAtomSymbols.contains(symbol)

  //assumes it is buildIn symbol, we can have only one fact per symbol
  def factForSymbol(symbol: String): Atom = facts.find(_.identifier == symbol).getOrElse(Atom("", List.empty))      //getOrElse will always produce a correct value so we don't care about the else part

  def rulesForAtom(currAtom: Atom): List[Rule] = rules.filter(_.resultAtom.identifier == currAtom.identifier)
}


object Parser{    //white space sensitive

  def isIdentifier(str: String): Boolean = {
    str.head.isLetter && str.head.isLower && (str.tail.count(x => x.isLetter || x.isDigit) == str.tail.length)
  }

  def isVariable(str: String): Boolean = {
    str.head.isLetter && str.head.isUpper && (str.tail.count(x => x.isLetter || x.isDigit) == str.tail.length)
  }

  def isConstant(str: String): Boolean = {
    isIdentifier(str)
  }

  def isListOfTerms(str: String): Boolean = {   //checks whether a string represents terms separated by commas
    val list = str.split(',').toList

    list.count(x => isTerm(x)) == list.length
  }

  def isTerm(str: String): Boolean = {
    isConstant(str) || isVariable(str) || {

      val firstBracketInx = str.indexWhere(_ == '(')
      val lastBracketInx = str.length - 1

      val name = str.slice(0, firstBracketInx)
      val arguments = str.slice(firstBracketInx + 1, lastBracketInx)   //without including the brackets

      isIdentifier(name) && isListOfTerms(arguments)
    }
  }

  def isAtom(str: String): Boolean = {

    !isConstant(str) && !isVariable(str) && isTerm(str)
  }

  def isFact(str: String): Boolean = {      //assuming not empty string
    str.last == '.' && {
      val newStr = str.take(str.length - 1)

      isAtom(newStr)
    }
  }

  def isListOfAtoms(str: String): Boolean = {
    val list = str.split(',').toList

    list.count(x => isAtom(x)) == list.length
  }

  def isRule(str: String): Boolean = {    //assuming not empty string
    str.last == '.' && {
      val newStr = str.take(str.length - 1)

      val parts  = newStr.split(":-")

      parts.length == 2 && isAtom(parts(0)) && isListOfAtoms(parts(1))
    }
  }

  def parse(fileName: String): Program = {

    val bufferedSource = Source.fromFile(fileName)

    var strFacts = List[String]()
    var strRules = List[String]()

    for (line <- bufferedSource.getLines if !line.isEmpty ) {
      if( isFact(line) )
        strFacts = line :: strFacts
      if( isRule(line) )
        strRules = line :: strRules
    }

    bufferedSource.close

    val facts = strFacts map {x => {val factStr = x.take(x.length - 1)
      AtomOps(factStr).atom}}

    val rules = strRules map {x => {val ruleStr = x.take(x.length - 1)
      RuleOps(ruleStr).rule}}

    Program(facts, rules)
  }
}


case class Atom(identifier: String, terms: List[String], substitutable: Boolean = true) {

  def == (other: Atom): Boolean = {
    this.identifier == other.identifier && this.terms == other.terms
  }

  def != (other: Atom): Boolean = !(this == other)

  def substitute(variablesSubstitution: Map[String, String]): Atom = {
    if (substitutable)
      Atom( identifier, terms.map(currTerm => Utilities.substituteTerm(currTerm, variablesSubstitution)) )
    else
      this
  }

  def isBad: Boolean = {
    terms.length == 2 && identifier == '='.toString && Utilities.areBadTerms(terms.head, terms.tail.head)
  }

  def isSolving: Boolean = {        //not the standard definition for solving equation(atom), which requires the variable not to be in any other equation as well
    terms.length == 2 && identifier == '='.toString && Parser.isVariable(terms.head) && Parser.isTerm(terms.tail.head)
  }

  override def toString: String = {
    identifier + '('.toString + terms.foldRight(')'.toString) {(x, y) => x + y}
  }

  def getVariables: List[String] = {
    terms.flatMap { term =>
      if (Parser.isVariable(term))
        List(term)
      else if (Parser.isAtom(term)){
        AtomOps(term).atom.getVariables
      }
      else    //term is constant => return empty list
        List.empty
    }
  }
}

case class AtomOps(str: String) {      //Atom == Predicate

  def identifier: String = {
    val firstBracketInx = str.indexWhere(_ == '(')

    str.slice(0, firstBracketInx)
  }

  def terms : List[String] = {
    val firstBracketInx = str.indexWhere(_ == '(')
    val lastBracketInx = str.length - 1

    val arguments = str.slice(firstBracketInx + 1, lastBracketInx)   //without including the brackets

    arguments.split(',').toList
  }

  def atom = Atom(identifier, terms)

}


case class Rule(resultAtom: Atom, inputAtoms: List[Atom]) {

  def substitute(variablesSubstitution: Map[String, String]): Rule = {
    Rule(resultAtom.substitute(variablesSubstitution),
      inputAtoms.map(currAtom => currAtom.substitute(variablesSubstitution)))
  }

}

case class RuleOps(str: String) {

  def resultAtom: Atom = {
    val parts  = str.split(":-")

    Atom ( AtomOps(parts(0)).identifier, AtomOps(parts(0)).terms )
  }

  def inputAtoms: List[Atom] = {
    val parts = str.split(":-")

    parts(1).split(',').toList.
      map(currAtomStr => AtomOps(currAtomStr).atom)
  }

  def rule = Rule(resultAtom, inputAtoms)

}


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

  def getSolutions: List[String] = {        //only if it is solving
    if (isSolving)
      constraints.map {
        case Atom("=", List(a, b), _ /* substitutable */) => a + '='.toString + b
      }
    else
      List[String]()
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

      val variablesForSubstitution = mutable.Queue[String]()

      val newConstraints = filteredConstraints.flatMap { constraint => {
        val first = constraint.terms.head         //first term (arg)
        val second = constraint.terms.tail.head   //second term (arg)

        if (Parser.isTerm(first) && !Parser.isVariable(first) && Parser.isVariable(second))                        //first solving transform
          List(Atom('='.toString, List(second, first)))

        else if (Parser.isAtom(first) && Parser.isAtom(second))                       //third non trivial solving transform
          Utilities.getAtomsComparing(AtomOps(first).atom, AtomOps(second).atom)

        else if (Parser.isVariable(first) && Parser.isTerm(second) && filteredState.deleteConstraint(constraint).containsVariable(first) ) {                //fourth solving transform => mark variable for substitution and mark current equation not to be substituted
          variablesForSubstitution.addOne(first)
          List(Atom(constraint.identifier, constraint.terms, substitutable = false))
        }

        else
          List(constraint)

      }
      }

      State(List.empty, newConstraints).substitute(Utilities.basicSubstitution(variablesForSubstitution.toList)).applyUnificationAlgorithm     //apply unification algorithm while we can (while we haven't reached solving or bad state)

    }

    else         //just do nothing
      this

  }

  def substitute(implicit variablesSubstitution: Map[String, String]): State = {
    State( queries.map(_.substitute(variablesSubstitution)), constraints.map(_.substitute(variablesSubstitution)) )
  }

}


object Utilities{

  def getAtomsComparing(a: Atom, b: Atom): List[Atom] = {      //f(x1,x2) = f(y5,c(10,arg2)) => List( =(x1,y5), =(x2,c(10,arg2)) )

    if(a.terms.nonEmpty && b.terms.nonEmpty && a.identifier == b.identifier)
    {
      val arg1 = a.terms.head
      val arg2 = b.terms.head
      Atom( '='.toString, List(arg1, arg2) ) :: getAtomsComparing(Atom(a.identifier, a.terms.tail), Atom(b.identifier, b.terms.tail))
    }
    else
      List.empty

  }

  def areBadTerms(a: String, b: String): Boolean = {          //bad code design, but readably shows when terms are bad for '='
    if (Parser.isConstant(a) && Parser.isConstant(b) && !( a == b ) )
      true
    else if ( (Parser.isAtom(a) && Parser.isConstant(b))
      || (Parser.isConstant(a) && Parser.isAtom(b)) )
      true
    else if ( Parser.isAtom(a) && Parser.isAtom(b) &&
      (!( AtomOps(a).atom.identifier == AtomOps(b).atom.identifier )          //either names are not the same
        || ( AtomOps(a).atom.terms.length != AtomOps(b).atom.terms.length) ) )       //or number of terms are different
      true
    else
      false

  }


  def basicSubstitution(variables: List[String]): Map[String, String] = {       //adds "1" to the variable name (i.e. x1 => x11)

    if(variables.nonEmpty)
      List( (variables.head, variables.head + 1.toString) ).toMap ++ basicSubstitution(variables.tail)
    else
      Map.empty
  }

  def substituteTerm(term: String, variableSubstitutions: Map[String, String]): String = {
    /*if(variables.isEmpty || substitutions.isEmpty || variables.length != substitutions.length)
      term
    else
      substituteTerm(term.replaceAll(variables.head, substitutions.head), variables.tail, substitutions.tail)*/

    if (Parser.isVariable(term) && variableSubstitutions.contains(term))
      variableSubstitutions(term)

    else if(Parser.isAtom(term)){
      AtomOps(term).atom.substitute(variableSubstitutions).toString
    }

    else    //term is constant => do nothing, just return the term itself
      term

  }

}


case class Solver(program: Program) {


  val resultSolutions: mutable.Queue[String] = mutable.Queue[String]()


  def solve(state: State, maxDepth: Int): Unit = {
    if(maxDepth > 0) {

      if (state.isSolving)
        resultSolutions.appendAll(state.getSolutions) //may fail appending, needs iterable

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

  def print(): Unit = println(resultSolutions)

}

object ParserTest extends App {
  //val currDir = new File(".").getAbsolutePath()               //to see where the pl files should be (facts and rules files)

  val program = Parser.parse("factsAndRules.txt")     //in the "Prolog-interpreter" folder

  val solver  = Solver(program)
  solver.solve( State( List(Atom("hasCar", List("X"))), List.empty), 2048 )
  solver.print()
}
