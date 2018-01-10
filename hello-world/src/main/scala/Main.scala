trait Types[T] {
        def subst(s: String => Type): T
}

sealed abstract class Type extends Types[Type] {
        def subst(s: String => Type): Type = this match {
                case Var(i) => s(i)
                case Int() => Int()
                case Arr(ty1, ty2) => Arr(ty1.subst(s), ty2.subst(s))
        }
}

case class Var(i: String) extends Type
case class Int() extends Type
case class Arr(ty1: Type, ty2: Type) extends Type

object Main extends App {
        println("Hello, Scala!")
}
