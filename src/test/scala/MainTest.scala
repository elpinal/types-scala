import org.scalatest.FunSuite

class MainTest extends FunSuite {
  test("Type.subst") {
    val s = Subst(Map("X" -> Type.Var("Y")))
    val t1 = Type.Var("X")
    val t2 = Type.Var("Y")
    assert(t1.subst(s) == t2)

    val t = Type.Var("Z")
    assert(t.subst(s) == t)
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
}
