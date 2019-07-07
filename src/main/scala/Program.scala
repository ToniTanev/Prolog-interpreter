

case class Program(facts: List[Atom], rules: List[Rule]) {
  def builtInAtomSymbols: List[String] = '='.toString :: ( for( fact <- facts ) yield fact.identifier )       //including "="

  def isBuiltInSymbol(symbol: String): Boolean = builtInAtomSymbols.contains(symbol)

  //assumes it is buildIn symbol, we can have only one fact per symbol
  def factForSymbol(symbol: String): Atom = facts.find(_.identifier == symbol).getOrElse(Atom("", List.empty))      //getOrElse will always produce a correct value so we don't care about the else part

  def rulesForAtom(currAtom: Atom): List[Rule] = rules.filter(_.resultAtom.identifier == currAtom.identifier)
}
