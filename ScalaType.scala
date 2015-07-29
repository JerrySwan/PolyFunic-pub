package polyfunic

/**
 * Represents a Scala type during the course of proof search.
 */
sealed trait ScalaType { 
  // TODO: Add support for Generics (~ higher-kind types).
}

/**
 * Represents an atomic Scala type that has a proper name (pretty much everything but functions).
 */
case class Named( get : String ) extends ScalaType {
  override def toString : String = get
}

/**
 * Represents a function type.
 */
case class Fun( args : List[ScalaType], ret : ScalaType ) extends ScalaType {
  override def toString : String = args.length match {
    case 1 => args.head + " => " + ret
    case _ => "(" + (args mkString ",") + ") => " + ret
  }
}