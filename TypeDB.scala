package polyfunic

/**
 * A DB entry for a data constructor.
 */
case class Constructor( name : String, args : List[ScalaType] ) {
  override def toString : String = {
    name + "(" + (args mkString ",") + ")"
  }
}

/**
 * A TypeDB assigns available constructors to type names. Generated by ExtractSealedHierarchy.
 */
case class TypeDB( ofMap : Map[String,List[Constructor]] ) {
  def byName( tn : Named ) : List[Constructor] = ofMap.getOrElse(tn.get, Nil)
}