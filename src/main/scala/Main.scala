trait Types[T] {
  def subst(s: Subst): T
}

case class Subst(m: Map[String, Type.Type]) {
  def getOrElse(i: String, x: Type.Type) = m getOrElse (i, x)

  def compoeseSubst(s: Subst) =
    Subst(m ++ s.m.mapValues(t => t.subst(this)))
}

object Type {
  sealed abstract class Type extends Types[Type] {
    def subst(s: Subst): Type = this match {
      case Var(i) => s.getOrElse(i, this)
      case Int => Int
      case Arr(ty1, ty2) => Arr(ty1.subst(s), ty2.subst(s))
    }
  }

  case class Var(i: String) extends Type
  case object Int extends Type
  case class Arr(ty1: Type, ty2: Type) extends Type
}

object Term {
  sealed abstract class Term extends Types[Term] {
    def subst(s: Subst): Term = this match {
      case Var(i) => this
      case Abs(t) => t.subst(s)
      case App(t1, t2) => App(t1.subst(s), t2.subst(s))
      case Ann(t, ty) => Ann(t.subst(s), ty.subst(s))
    }
  }

  case class Var(x: Int) extends Term
  case class Abs(t: Term) extends Term
  case class App(t1: Term, t2: Term) extends Term
  case class Ann(t: Term, ty: Type.Type) extends Term
}

case class Context(l: List[Type.Type]) extends Types[Context] {
  def subst(s: Subst): Context =
    Context(l.map(ty => ty.subst(s)))
}

object Main extends App {
  println("Hello, Scala!")

  println("type variable", Type.Var("x"))
  println("Int type", Type.Int)
  println("x -> Int", Type.Arr(Type.Var("x"), Type.Int))

  println("index of variable", Term.Var(0))
  println("λ.0", Term.Abs(Term.Var(0)))
  println("0 1", Term.App(Term.Var(0), Term.Var(1)))

  println("context", Context(List(Type.Int)))

  println("[x->y]x", Type.Var("x").subst(Subst(Map("x" -> Type.Var("y")))))
  println("[y->y]x", Type.Var("x").subst(Subst(Map("y" -> Type.Var("y")))))

  val s1 = Subst(Map("x" -> Type.Var("y")))
  val s2 = Subst(Map("y" -> Type.Var("z")))
  val s3 = Subst(Map("x" -> Type.Var("z")))
  println("[x->y] . [y->z]", s1.compoeseSubst(s2))
  println("[x->y] . [x->z]", s1.compoeseSubst(s3))
  println("[y->z] . [x->y]", s2.compoeseSubst(s1))
}
