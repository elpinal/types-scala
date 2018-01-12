import org.scalatest._
import prop._

class MainTest extends PropSpec with TableDrivenPropertyChecks with Matchers {
  property("Type.subst") {
    val s = Subst(Map("X" -> Type.Var("Y")))
    val t = Type.Var("Z")
    val t0 = Type.Var("v0")

    val examples =
      Table(
        ("ty1", "s", "ty2"),
        (Type.Var("X"), s, Type.Var("Y")),
        (t, s, t),
        (
          t0,
          Subst(Map("X" -> Type.Arr(Type.Var("X"), Type.Var("X")))),
          t0
        )
      )

    forAll(examples) { case (t1, s, t2) =>
      t1.subst(s) should be (t2)
    }
  }

  property("ConstraintTyping.getTypeAndConstraint") {
    val ctx = Context(List(Type.Var("A")))
    val t = Term.Abs(Type.Var("B"), Term.App(Term.Var(0), Term.Var(1)))
    val ty = Type.Arr(Type.Var("B"), Type.Var("v0"))
    val cs = Constraint.set(Type.Var("B") -> Type.Arr(Type.Var("A"), Type.Var("v0")))
    assert(ConstraintTyping.getTypeAndConstraint(ctx, t) == Right(ty, cs))
  }

  property("unify Set of Constraint") {
    val t = Type.Arr(Type.Var("A"), Type.Var("v0"))
    val cs = Constraint.set(Type.Var("B") -> t)
    val s = Subst(Map("B" -> t))
    assert(Constraint.unify(cs) == Right(s))
  }

  property("PrincipalType.fromTermWithContext") {
    val t = Term.App(
      Term.Abs(Type.Var("X"), Term.Var(0)),
      Term.Abs(Type.Var("Y"), Term.Var(0))
    )
    val ctx = Context(List())
    val tyYY = Type.Arr(Type.Var("Y"), Type.Var("Y"))
    val s = Subst(Map(
      "X" -> tyYY,
      "v0" -> tyYY
      ))
    val ty = tyYY
    assert(PrincipalType.fromTermWithContext(ctx, t) == Right((s, ty)))
  }
}
