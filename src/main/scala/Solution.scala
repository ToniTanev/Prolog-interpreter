

case class Solution(variables: List[String], values: List[String]) {  //assume there is bijection between variables and values

  def printStringRec(variables: List[String], values: List[String]): String = {
    if (variables.nonEmpty && values.nonEmpty) {
      if (variables.tail.nonEmpty && values.tail.nonEmpty)
        variables.head + " = " + values.head + ", " + printStringRec(variables.tail, values.tail)
      else
        variables.head + " = " + values.head
    }
    else
      ""
  }

  def printString: String = printStringRec(this.variables, this.values)

  def print(): Unit = println(printString)
}
