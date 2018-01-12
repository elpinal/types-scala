import org.scalatest.FunSuite

class MainTest extends FunSuite {
  test("Type.subst") {
    val s = Subst(Map("X" -> Type.Var("Y")))
    val t1 = Type.Var("X")
    val t2 = Type.Var("Y")
    assert(t1.subst(s) == t2)

    val t = Type.Var("Z")
    assert(t.subst(s) == t)

    val t0 = Type.Var("v0")
    val s1 = Subst(Map("X" -> Type.Arr(Type.Var("X"), Type.Var("X"))))
    assert(t0.subst(s1) == t0)
  }

  test("ConstraintTyping.getTypeAndConstraint") {
    val ctx = Context(List(Type.Var("A")))
    val t = Term.Abs(Type.Var("B"), Term.App(Term.Var(0), Term.Var(1)))
    val ty = Type.Arr(Type.Var("B"), Type.Var("v0"))
    val cs = Constraint.set(Type.Var("B") -> Type.Arr(Type.Var("A"), Type.Var("v0")))
    assert(ConstraintTyping.getTypeAndConstraint(ctx, t) == Right(ty, cs))
  }

  test("unify Set of Constraint") {
    val t = Type.Arr(Type.Var("A"), Type.Var("v0"))
    val cs = Constraint.set(Type.Var("B") -> t)
    val s = Subst(Map("B" -> t))
    assert(Constraint.unify(cs) == Right(s))
  }

  test("PrincipalType.fromTermWithContext") {
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
