import scala.language.implicitConversions

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
