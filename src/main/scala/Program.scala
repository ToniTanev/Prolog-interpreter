

case class Program(facts: List[Atom], rules: List[Rule]) {
  def builtInAtomSymbols: List[String] = '='.toString :: ( for( fact <- facts ) yield fact.identifier )       //including "="

  def isBuiltInSymbol(symbol: String): Boolean = builtInAtomSymbols.contains(symbol)

  //assumes it is buildIn symbol, we can have only one fact per symbol
  def factsForSymbol(symbol: String): List[Atom] = facts.filter(_.identifier == symbol)

  def rulesForAtom(currAtom: Atom): List[Rule] = rules.filter(_.resultAtom.identifier == currAtom.identifier)
}
