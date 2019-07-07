
import scala.io.Source


object Parser{    //white space sensitive

  def isIdentifier(str: String): Boolean = {
    if(str.nonEmpty)
      str.head.isLetter && str.head.isLower && (str.tail.count(x => x.isLetter || x.isDigit) == str.tail.length)
    else
      false
  }

  def isVariable(str: String): Boolean = {
    if(str.nonEmpty)
      str.head.isLetter && str.head.isUpper && (str.tail.count(x => x.isLetter || x.isDigit) == str.tail.length)
    else
      false
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

  def numBrackets(str: String): Int = {
    str.count(_ == '(') - str.count(_ == ')')
  }

  def areBracketsFineRec(str: String, cnt: Int): Boolean = {
    if (cnt < 0)
      false
    else{
      if(str.isEmpty){
        cnt == 0
      }
      else{
        if (str.head == '(')
          areBracketsFineRec(str.tail, cnt + 1)
        else if (str.head == ')')
          areBracketsFineRec(str.tail, cnt - 1)
        else
          areBracketsFineRec(str.tail, cnt)
      }
    }
  }

  def areBracketsFine(str: String): Boolean = areBracketsFineRec(str, 0)

  def splitIndex(str: String): Int = {          //splits to two parts, with the first being a potential atom; returns -1 if good split index isn't found

    for (i <- 0 until str.length) {
      val currChar = str.charAt(i)
      val prefix = str.splitAt(i)._1

      if (currChar == ',' && areBracketsFine(prefix) ) {
        return i
      }
    }

    -1
  }

  def splitToListOfAtoms(str: String): List[String] = {
    val splitIdx = splitIndex(str)

    if (splitIdx != -1) {
      val parts = str.splitAt( splitIdx )

      parts._1 :: splitToListOfAtoms(parts._2.tail /* don't include the comma */)
    }
    else
      List(str)
  }

  def isListOfAtoms(str: String): Boolean = {
    val list = splitToListOfAtoms(str)

    list.nonEmpty /* require at least one atom */ && list.count(x => isAtom(x)) == list.length
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

