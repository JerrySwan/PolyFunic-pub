package polyfunic

/**
  Represents a completed proof tree output of the proof search process.
*/
sealed trait ProofTreeLike {
  val root : SequentLike
  val branches : List[ProofTreeLike]
}
case class ProofTree( root : Context, branches : List[ProofTreeLike] ) extends ProofTreeLike
case class ProofTreeRightNamed( root : Sequent, idx : Int, branches : List[ProofTreeLike] ) extends ProofTreeLike
case class ProofTreeRightFun( root : Sequent, branches : List[ProofTreeLike] ) extends ProofTreeLike
case class ProofTreeAxiom( root : Context ) extends ProofTreeLike {
  override val branches : List[ProofTreeLike] = Nil
}

/**
  Contains all information required by the Proof Search process.
  Implements a generalization of the LJT proof search procedure described in
  Contraction-Free Sequent Calculi for Intuitionistic Logic. Author(s): Roy Dyckhoff. Source: The Journal of Symbolic Logic, Vol. 57, 1992.
 */
case class Search(db : TypeDB) {
  
  // A synonym for searchStep.
  def apply( to : Sequent ) : BT[ProofTreeLike] = searchStep(to)
  
  /**
    Applies left and right rules until 
    a.) Success: A proof is found, or
    b.) Failure: All possible choices are exhausted. 
   */
  def searchStep( sequent : Sequent ) : BT[ProofTreeLike] = rightStep(sequent) orElse (sequent.choices flatMap leftStep) orElse (sequent.choices flatMap axiomStep)
  def continue( on : List[Sequent] ) : BT[List[ProofTreeLike]] = {
    val branches : List[BT[ProofTreeLike]] = on.map(searchStep)
    BT.build(branches)
  }
  
  /**
    Applies a right rule if possible, fails otherwise.
   */
  def rightStep( sequent : Sequent ) : BT[ProofTreeLike] = sequent.rhs match {
    case (t : Named) => BT.oneOf(db.byName(t).zipWithIndex) flatMap { (ctor : (Constructor,Int)) => 
      val targets = ctor._1.args.map( arg => sequent.replace(arg) )
      for { b <- continue(targets) } yield ProofTreeRightNamed( sequent, ctor._2, b )
    }
    case Fun(args,ret) => {
      val targets = List( sequent.add(args).replace(ret) )
      for { b <- continue(targets) } yield ProofTreeRightFun( sequent, b )
    }
  }
  
  /**
    Applies a left rule if possible, fails otherwise.
   */
  def leftStep( context : Context ) : BT[ProofTreeLike] = context.focus match {
    case (t : Named) => db.byName(t) match {
      case Nil   => Failure()
      case ctors => {
        val targets = ctors.map( c => context.add(c.args:_*).unfocus )
        for { b <- continue(targets) } yield ProofTree( context, b )
      }
    }
    case Fun(args,ret) => args match {
      case Nil => {
        val targets = List( context.add(ret).unfocus )
        for { b <- continue(targets) } yield ProofTree( context, b )
      }
      case (a : Named) :: args => db.byName(a) match {
          case Nil => if (context.lhs contains a) { // the MP case!
            val targets = List( context.add(Fun(args,ret)).unfocus )
            for { b <- continue(targets) } yield ProofTree( context, b )
          } else Failure()
          case ctors => {
            val targets = List( context.add(ctors.map(c => Fun( c.args ++ args, ret)):_*).unfocus )
            for { b <- continue(targets) } yield ProofTree( context, b )
          }
      }
      case (f : Fun) :: args => {
        val targets = List( context.add( Fun( f.ret :: args, ret) ).replace(f).unfocus, context.add(Fun(args,ret)).unfocus )
        for { b <- continue(targets) } yield ProofTree( context, b )
      }
    }
  }
  
  /**
    Applies an axiom rule if possible, fails otherwise.
   */
  def axiomStep( context : Context ) : BT[ProofTreeLike] = if (context.focus == context.rhs) {
    BT.unit( ProofTreeAxiom( context ) )
  } else Failure()
  
 
}
