import scala.language.implicitConversions

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

trait Monoid[A] {
  def add(x: A, y: A): A
  def unit: A
}

object ImplicitMonoids {
  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def add(x: String, y: String): String = x concat y
    def unit: String = ""
  }

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def add(x: Int, y: Int): Int = x + y
    def unit: Int = 0
  }
}

object Main extends App {
  import ImplicitConversions._
  import ImplicitMonoids._

  def sum(a: Int, b: Int): Int = sum(List[Int](a, b))

  def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
    if (xs.isEmpty) m.unit
    else m.add(xs.head, sum(xs.tail))

  println(f"${sum(1, 1.3f)}")
  println(f"${sum(1, 1)}")
  println(sum(List(1, 2, 3))) // uses intMonoid implicitly
  println(
    sum[Int](List(1.1f, 2.0f, 3.0f))
  ) // uses intMonoid implicitly but first parses implicitly

  println(
    sum[Int](List[Container](Container(1.1f)))
  ) // uses intMonoid implicitly but first parses implicitly

  println(sum(List("a", "b", "c"))) // uses stringMonoid implicitly
}
