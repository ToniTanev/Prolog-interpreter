

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

    Parser.splitToListOfAtoms(parts(1)).map(currAtomStr => AtomOps(currAtomStr).atom)

  }

  def rule = Rule(resultAtom, inputAtoms)

}
