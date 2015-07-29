package polyfunic

/**
  Represents a sequent in a proof tree.
 */
sealed trait SequentLike {
  val lhs : List[ScalaType]
  val rhs : ScalaType
}

/**
  Represents a sequent with two contexts (lefts - Gamma, rights - Delta) and a primary formula on the left hand side.
 */
case class Context( lefts : List[ScalaType], focus : ScalaType, rights : List[ScalaType], rhs : ScalaType ) extends SequentLike {
  override def toString : String = {
    val lhs : List[String] = lefts.map(_.toString) ++ (("[" + focus.toString + "]") :: rights.map(_.toString))
    (lhs mkString ",") + " |- " + rhs
  }
  
  override val lhs = lefts ++ (focus :: rights)
  
  def furtherChoices : BT[Context] = BT.unit(this) interleave { rights match {
    case Nil       => Failure()
    case (newFocus :: newRights) => {
      Context( lefts :+ focus, newFocus, newRights, rhs ).furtherChoices
    }}
  }
  
  def add( xs : ScalaType* ) : Context = Context( xs.toList ++ lefts, focus, rights, rhs )
  def replace( x : ScalaType ) : Context = Context( lefts, focus, rights, x )
  def unfocus : Sequent = Sequent( lefts ++ rights, rhs )
}

/**
  Represents a sequent not divided into contexts on the left hand side.
 */
case class Sequent( lhs : List[ScalaType], rhs : ScalaType ) extends SequentLike {
  override def toString : String = {
    (lhs mkString ",") + " |- " + rhs
  }
  
  def add( xs : List[ScalaType] ) : Sequent = Sequent( xs ++ lhs, rhs )
  def replace( rhs : ScalaType )  : Sequent = Sequent( lhs, rhs )
  
  /**
    Represents all possible choices of contexts on the left hand side.
   */
  def choices : BT[Context] = lhs match {
    case Nil => Failure()
    case (focus :: rights) => Context(Nil,focus,rights,rhs).furtherChoices
  }
}
