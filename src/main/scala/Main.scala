trait Types[T] {
  def subst(s: Subst): T
}

case class Subst(m: Map[String, Type.Type]) {
  def getOrElse(i: String, x: Type.Type) = m getOrElse (i, x)

  def compoeseSubst(s: Subst) =
    Subst(m ++ s.m.mapValues(t => t.subst(this)))
}

object Subst {
  def empty = Subst(Map())
}

object Type {
  sealed abstract class Type extends Types[Type] {
    def subst(s: Subst): Type = this match {
      case Var(i)        => s.getOrElse(i, this)
      case Int           => Int
      case Arr(ty1, ty2) => Arr(ty1.subst(s), ty2.subst(s))
    }

    def fromVar() = this match {
      case Var(i) => Some(i)
      case _      => None
    }

    def fromArr() = this match {
      case Arr(ty1, ty2) => Some(ty1 -> ty2)
      case _             => None
    }
  }

  case class Var(i: String) extends Type
  case object Int extends Type
  case class Arr(ty1: Type, ty2: Type) extends Type
}

object Term {
  sealed abstract class Term extends Types[Term] {
    def subst(s: Subst): Term = this match {
      case Var(n)      => this
      case Abs(ty, t)  => Abs(ty.subst(s), t.subst(s))
      case App(t1, t2) => App(t1.subst(s), t2.subst(s))
      case Ann(t, ty)  => Ann(t.subst(s), ty.subst(s))
    }
  }

  case class Var(n: Int) extends Term
  case class Abs(ty: Type.Type, t: Term) extends Term
  case class App(t1: Term, t2: Term) extends Term
  case class Ann(t: Term, ty: Type.Type) extends Term
}

case class Context(l: List[Type.Type]) extends Types[Context] {
  def subst(s: Subst): Context =
    Context(l map { _.subst(s) })

  def has(n: Int) = l isDefinedAt n
  def get(n: Int) = l apply n
  def add(ty: Type.Type) = Context(ty :: l)
}

object Constraint {
  case class Constraint(ty1: Type.Type, ty2: Type.Type)
      extends Types[Constraint] {
    def subst(s: Subst): Constraint =
      Constraint(ty1.subst(s), ty2.subst(s))
  }

  def set(cs: (Type.Type, Type.Type)*) =
    cs.map({ case (ty1, ty2) => Constraint(ty1, ty2) }).toSet

  def empty = set()

  def unify(cs: Set[Constraint]): Either[String, Subst] = {
    val rest = cs drop 1
    cs.headOption map {
      case Constraint(s, t) =>
        if (s == t) {
          unify(rest)
        } else {
          varBind(rest, s, t) orElse varBind(rest, t, s) orElse arrBind(
            rest,
            s,
            t) getOrElse Left(s"cannor unify: $s and $t")
        }
    } getOrElse Right(Subst.empty)
  }

  def varBind(cs: Set[Constraint], ty1: Type.Type, ty2: Type.Type) =
    ty1.fromVar() map { v =>
      val sub = Subst(Map(v -> ty2))
      unify(cs map { _.subst(sub) }) map { _.compoeseSubst(sub) }
    }

  def arrBind(cs: Set[Constraint], tyS: Type.Type, tyT: Type.Type) =
    for {
      (tyS1, tyS2) <- tyS.fromArr()
      (tyT1, tyT2) <- tyT.fromArr()
    } yield unify(cs + Constraint(tyS1, tyT1) + Constraint(tyS2, tyT2))
}

object ConstraintTyping {
  def getTypeAndConstraint(
      ctx: Context,
      t: Term.Term): Either[String, (Type.Type, Set[Constraint.Constraint])] = {
    val i = new Internal()
    i.traverse(ctx, t)
  }

  class Internal {
    private var count = 0

    def traverse(ctx: Context, t: Term.Term)
      : Either[String, (Type.Type, Set[Constraint.Constraint])] = {
      t match {
        case Term.Var(n) if ctx.has(n) => Right((ctx.get(n), Constraint.empty))
        case Term.Var(n)               => Left(s"no such variable in context: $n")
        case Term.Abs(ty, t) =>
          traverse(ctx add ty, t).map({
            case (ty1, cs) => (Type.Arr(ty, ty1), cs)
          })
        case Term.App(t1, t2) =>
          for {
            r1 <- traverse(ctx, t1)
            r2 <- traverse(ctx, t2)
          } yield app(r1, r2)
      }
    }

    def app(r1: (Type.Type, Set[Constraint.Constraint]),
            r2: (Type.Type, Set[Constraint.Constraint])) = {
      val v = freshVar()
      val (ty1, cs1) = r1
      val (ty2, cs2) = r2
      val c = Constraint.set(ty1 -> Type.Arr(ty2, v))
      (v, cs1 ++ cs2 ++ c)
    }

    private def freshVar() = {
      val n = count
      count += 1
      Type.Var(s"v$n")
    }
  }
}

object PrincipalType {
  def fromTermWithContext(ctx: Context, t: Term.Term) =
    ConstraintTyping.getTypeAndConstraint(ctx, t) flatMap {
      case (ty, cs) =>
        Constraint.unify(cs) map { s =>
          (s, ty.subst(s))
        }
    }
}

object Main extends App {
  println("Hello, Scala!")

  println("type variable", Type.Var("x"))
  println("Int type", Type.Int)
  println("x -> Int", Type.Arr(Type.Var("x"), Type.Int))

  println("index of variable", Term.Var(0))
  println("λ:Int.0", Term.Abs(Type.Int, Term.Var(0)))
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

  println("constraint", Constraint.set())
  println("constraint", Constraint.set(Type.Int -> Type.Int))
  println("constraint",
          Constraint.set(Type.Int -> Type.Int, Type.Int -> Type.Int))
  println("constraint",
          Constraint.set(Type.Int -> Type.Int, Type.Int -> Type.Var("x")))

  val ctx = Context(List())

  {
    val t = Term.Var(0)
    println("typing", ConstraintTyping.getTypeAndConstraint(ctx, t))

    {
      val ctx = Context(List(Type.Var("X")))
      println("typing", ConstraintTyping.getTypeAndConstraint(ctx, t))
    }
  }

  {
    val t = Term.Abs(Type.Int, Term.Var(0))
    println("typing", ConstraintTyping.getTypeAndConstraint(ctx, t))
  }

  {
    val t = Term.Abs(Type.Var("X"), Term.Var(0))
    println("typing", ConstraintTyping.getTypeAndConstraint(ctx, t))
  }

  {
    val t = Term.Abs(Type.Var("X"), Term.Var(0))
    val t1 = Term.App(t, t)
    println("typing", ConstraintTyping.getTypeAndConstraint(ctx, t1))
  }

  {
    val ctx = Context(List(Type.Var("Y")))
    val t = Term.Abs(Type.Var("X"), Term.Var(0))
    val t1 = Term.App(t, Term.Var(0))
    println("typing", ConstraintTyping.getTypeAndConstraint(ctx, t1))
  }
}
