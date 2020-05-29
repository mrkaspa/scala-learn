case class Container(num: Float)

trait Mapper[A, B] {
  def map(a: A): B
}

object ImplicitConversions {
  implicit val containerToInt: Mapper[Container, Int] =
    new Mapper[Container, Int] {
      def map(cont: Container) = cont.num.toInt
    }

  implicit def floatToInt(x: Float) =
    x.toInt

  implicit def listAToB[A, B](ls: List[A])(implicit m: Mapper[A, B]): List[B] =
    ls.map(m.map)
}
