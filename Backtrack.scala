package polyfunic

/**
 * A simple but safe backtracking monad (interleave on bind).
 */
sealed trait BT[A] {
  def interleave( ys : => BT[A] ) : BT[A]
  def flatMap[B]( f : A => BT[B] ) : BT[B]
  def map[B]( f : A => B ) : BT[B] = flatMap(BT unit f(_))
  val isEmpty : Boolean
  def then( attempt : => BT[A] ) : BT[A]
  def orElse( attempt : => BT[A] ) : BT[A]
}

case class Failure[A]() extends BT[A] {
  override def interleave( ys : => BT[A] ) : BT[A] = ys 
  override def flatMap[B]( f : A => BT[B] ) : BT[B] = Failure()
  override val isEmpty : Boolean = true
  override def then( attempt : => BT[A] ) : BT[A] = attempt
  override def orElse( attempt : => BT[A] ) : BT[A] = attempt
}


case class Success[A]( first : A, more : () => BT[A] ) extends BT[A] {
  override def interleave( ys : => BT[A] ) : BT[A] = Success( first, () => ys interleave more() )
  override def flatMap[B]( f : A => BT[B] ) : BT[B] = f(first) interleave ( (more()) flatMap f )
  override val isEmpty : Boolean = false
  override def then( attempt : => BT[A] ) : BT[A] = Success( first, () => more() then attempt )
  override def orElse( attempt : => BT[A] ) : BT[A] = this
}

object BT {
  def unit[A]( a : A ) : BT[A] = Success( a, () => Failure() )
  
  def oneOf[A]( xs : List[A] ) : BT[A] = xs match {
    case Nil       => Failure()
    case (x :: xs) => Success( x, () => oneOf(xs) )
  }
  
  def unsafeToList[A]( a : BT[A] ) : List[A] = a match {
    case Failure()            => Nil
    case Success(first, more) => first :: unsafeToList( more() )
  }
  
  def unsafeHead[A]( a : BT[A] ) : A = unsafeToList(a).head
  
  def build[T]( from : List[BT[T]] ) : BT[List[T]] = from match {
    case Nil        =>  Success( Nil, () => Failure() )
    case (x :: Nil) => x flatMap { (p : T) => BT unit List(p) }
    case (x :: xs)  => x flatMap { (p : T) => build(xs).map(p :: _) }
  }
}

