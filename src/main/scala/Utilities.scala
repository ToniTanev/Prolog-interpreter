

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
