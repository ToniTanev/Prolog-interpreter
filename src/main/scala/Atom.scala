

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
    identifier + '('.toString + terms.foldRight(')'.toString) { (x, y) => {
      if (y != ")") x + "," + y
      else          x + y
    }
    }
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
