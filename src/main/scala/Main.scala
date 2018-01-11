trait Types[T] {
  def subst(s: String => Type.Type): T
}

object Type {
  sealed abstract class Type extends Types[Type] {
    def subst(s: String => Type): Type = this match {
      case Var(i) => s(i)
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
    def subst(s: String => Type.Type): Term = this match {
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
  def subst(s: String => Type.Type): Context =
    Context(l.map({ty => ty.subst(s)}))
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
}
