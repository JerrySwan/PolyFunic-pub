package polyfunic.backend

import polyfunic._

case object LaTeX {  
  
  def branchPoint( branches : Int ) : String => String = branches match {
    case 0 => (s => "\\AxiomC{" + s + "}" )
    case 1 => (s => "\\UnaryInfC{" + s + "}" )
    case 2 => (s => "\\BinaryInfC{" + s + "}" )
    case 3 => (s => "\\TrinaryInfC{" + s + "}" ) // should be ternary, but bussproofs :(
    case 4 => (s => "\\QuaternaryInfC{" + s + "}" )
    case 5 => (s => "\\QuinaryInfC{" + s + "}" )
    case _ => throw new Exception("This backend does not support superquinary inference rules")
  }
  
  def name(t : ScalaType) : String = t match {
    case Fun(Nil,b)     => name(b)
    case Fun(List(a),b) => name(a) + "\\rightarrow " + name(b)
    case Fun(as,b)      => "(" + (as.map(name) mkString ",") + ")\\rightarrow " + name(b)
    case Named(n)       => n.toUpperCase
  }
  
  def sequent( s : SequentLike ) : String = "$" + s.lhs.map(name).mkString(",") + "\\vdash " + name(s.rhs) + "$"
  
  def rightLabel(t : ScalaType, ix : Int) : String = "\\RightLabel{\\scriptsize($" + name(t) + "-R_{" + ix + "}$)}"
  def rightLabel(t : ScalaType) : String = "\\RightLabel{\\scriptsize($" + name(t) + "-R$)}"
  val rightLabelFun : String = "\\RightLabel{\\scriptsize($\\rightarrow -R$)}"
  def leftLabel(t : ScalaType) : String = "\\RightLabel{\\scriptsize($" + name(t) + "-L$)}"
  val leftLabelFun : String = "\\RightLabel{\\scriptsize($\\rightarrow -L$)}"
  
  def body( pt : ProofTreeLike ) : List[String] = pt.branches.flatMap(body) ++ (pt match {
    case (pt : ProofTreeRightNamed) =>
      List( rightLabel(pt.root.rhs, pt.idx)
          , branchPoint(pt.branches.size)( sequent(pt.root) )
          )
    case (pt : ProofTreeRightFun)   =>
      List( rightLabelFun
          , branchPoint(pt.branches.size)( sequent(pt.root) )
          )
    case (pt : ProofTree)           => 
      List( pt.root.focus match { case (t : Named) => leftLabel(t); case _ => leftLabelFun }
          , branchPoint(pt.branches.size)( sequent(pt.root) )
          )
    case (pt : ProofTreeAxiom)      =>
      List( branchPoint(pt.branches.size)( sequent(pt.root) )
          )
  })
  
  /**
   * Generates LaTeX/bussproofs output from the given proof tree.
   */
  def apply( pt : ProofTreeLike ) : String = "\\begin{prooftree}\n" + (body(pt) mkString "\n") + "\n\\end{prooftree}"
  
}
