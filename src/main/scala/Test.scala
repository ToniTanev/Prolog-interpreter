

object ParserTest extends App {
  //val currDir = new File(".").getAbsolutePath()               //to see where the pl files should be (facts and rules files)

  val program = Parser.parse("factsAndRules.txt")     //in the "Prolog-interpreter" folder

  val solver  = Solver(program, List("X"))
  solver.solve( State( List(Atom("p", List("a"))), List.empty), 1024 )
  solver.print()

  println(Atom("atom", List("a(b(X),X)", "d(Y,e(Y))")).substitute(List("X"->"Y", "Y"->"Z").toMap))
}
