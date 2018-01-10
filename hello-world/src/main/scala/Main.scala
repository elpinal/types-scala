sealed abstract class Type
case class Var(i: String) extends Type
case object Int extends Type
case class Arr(ty1: Type, ty2: Type) extends Type

object Main extends App {
        val subst: (String => Type, Type) => Type = (s: String => Type, ty: Type) => ty match {
                case Var(i) => s(i)
                case Int => Int
                case Arr(ty1, ty2) => Arr(subst(s, ty1), subst(s, ty2))
        }

        println("Hello, Scala!")
}
