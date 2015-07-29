package polyfunic


/**
 * Example environment for PolyFunic. Contains several predefined types.
 */
object Example {

  // Step 1. Build a TypeDB. This contains information about the types in a given program.
  
  val I : ScalaType = Named("Integer")
  val S : ScalaType = Named("String")
  
  // either Int or String
  val EISLeft  = Constructor("Left", List(I))
  val EISRight = Constructor("Right", List(S))
  val EIS : ScalaType = Named( "EIS" )
  
  // both Int and String
  val BISCons = Constructor("Both", List(I,S))
  val BIS : ScalaType = Named( "BIS" )
  
  // function from String to Int and vice versa
  val IfI : ScalaType = Fun( List( I ), I )
  val IfS : ScalaType = Fun( List( I ), S )
  
  // function from EIS to Int and vice versa
  val IfEIS : ScalaType = Fun( List(I), EIS )
  val EISfI : ScalaType = Fun( List(EIS), I )
  
  // function from BIS to Int and vice versa
  val BISfI : ScalaType = Fun( List(BIS), I )
  val IfBIS : ScalaType = Fun( List(I), BIS )
  
  // function from Int and String to BIS
  val ISfBIS : ScalaType = Fun( List(S,I), BIS )
  
  val descDB : Map[String,List[Constructor]] = List( "EIS" -> List(EISLeft,EISRight)
                                                   , "BIS" -> List(BISCons) 
                                                   ).toMap
  object DB extends TypeDB(descDB)
  
  
  // valid example sequents
  val valid1 : Sequent = Sequent( List(BIS), I )
  val valid2 : Sequent = Sequent( List(BIS), S )
  val valid3 : Sequent = Sequent( List(I), EIS )
  val valid4 : Sequent = Sequent( List(S), EIS )
  val valid5 : Sequent = Sequent( List(BIS), EIS )
  val valid6 : Sequent = Sequent( List(I,IfBIS),S )
  val valid7 : Sequent = Sequent( List(S,IfBIS),S )
  val valid8 : Sequent = Sequent( List(I,IfBIS),I )
  val valid9 : Sequent = Sequent( List(EISfI,EIS), I )
  val valid0 : Sequent = Sequent( List(EISfI,IfBIS,EIS), S )
  val validA : Sequent = Sequent( List(S), IfBIS )
  
  // invalid example statements
  val tc1 : Sequent = Sequent( List(IfI,EIS), I )
  val tc2 : Sequent = Sequent( List(I,IfI), I )
  
  def main( args : Array[String] ) : Unit = {
    val input = validA // experiment with different sequents by changing this line
    println( "attempting to prove: " + input + "\n")
    val result = Search(DB)(input)
    println( "\n" + result  )
    
    // NOTE: Scala codegen is stripped from this version
    // println( "\n CODE \n")
    // val code : BT[String] = for { r <- result } yield polyfunic.backend.Scala(DB)(r, Nil)
    // println( BT unsafeHead code )
    
    println( "\n LATEX/BUSSPROOFS TREE \n")
    val latex : BT[String] = for {r <- result } yield polyfunic.backend.LaTeX(r)
    println( BT unsafeHead latex )
  }
}
