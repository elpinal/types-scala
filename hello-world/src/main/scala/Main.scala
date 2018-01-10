sealed abstract class Type
case class Var(i: String) extends Type
case object Int extends Type
case class Arr(ty1: Type, ty2: Type) extends Type

object Main extends App {
  println("Hello, Scala!")
}
