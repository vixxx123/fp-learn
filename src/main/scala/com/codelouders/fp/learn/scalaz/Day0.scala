package com.codelouders.fp.learn.scalaz

/**
  * Created by Wiktor on 25/04/2017.
  */
object Day0 extends App {


  object AdHocPoly {

    trait Plus[T] {
      def plus(a1: T, a2: T): T
    }

    def plus[T : Plus](a1: T, a2: T): T = implicitly[Plus[T]].plus(a1, a2)
  }

  implicit object IntPlus extends AdHocPoly.Plus[Int] {
    override def plus(a1: Int, a2: Int): Int = a1 + a2
  }

  implicit object StringPlus extends AdHocPoly.Plus[String] {
    override def plus(a1: String, a2: String): String = s"$a1$a2"
  }

  println(s"AdHocPoly int: ${AdHocPoly.plus(1, 2)}")
  println(s"AdHocPoly string: ${AdHocPoly.plus("1", "2")}")

  object IntMonoid {
    def mappend(a: Int, b:Int): Int = a + b
    def zero: Int = 0
  }

  object StringMonoid {
    def mappend(a: String, b: String): String = a + b
    def zero: String = ""
  }

  val data = List(1, 4, 5, 7, 10)
  val dataString = List("W", "i", "k", "t", "o", "r")
  println(s"Monoid int: ${data.foldLeft(IntMonoid.zero)(IntMonoid.mappend)}")
  println(s"Monoid string: ${dataString.foldLeft(StringMonoid.zero)(StringMonoid.mappend)}")

  //Monoid functional structure / typeclass / identity + associative laws
  trait Monoid[T] {
    def mappend(a: T, b: T): T
    def zero: T
  }

  object Monoid {
    implicit val intMonoid = new Monoid[Int] {
      override def mappend(a: Int, b: Int): Int = a + b
      override def zero: Int = 0
    }

    implicit val stringMonoid = new  Monoid[String] {
      override def mappend(a: String, b: String): String = a + b
      override def zero: String = ""
    }

    implicit val listMonoid = new  Monoid[String] {
      override def mappend(a: String, b: String): String = a + b
      override def zero: String = ""
    }
  }

  /*
   * More generic. Now sum can sum any object for which we have Monoid
   */
  def sum[T](data: List[T], monoid: Monoid[T]): T = data.foldLeft(monoid.zero)(monoid.mappend)
  println(s"sum int: ${sum(data, Monoid.intMonoid)}")

  /**
    * Further generalisation with ad-hoc polymorphism
    */
  def genSum[T : Monoid](data: List[T]): T = {
    val monoid = implicitly[Monoid[T]]
    data.foldLeft(monoid.zero)(monoid.mappend)
  }

  println(s"genSum ${genSum(data)}")

  trait FoldLeft[F[_]] {
    def foldLeft[A, B](xs: F[A], zero: B, f: (B, A) ⇒ B): B
  }

  object FoldLeft {

    def apply[M[_]: FoldLeft]: FoldLeft[M] = implicitly
    
    implicit val intFoldLeft = new FoldLeft[List] {
      override def foldLeft[A, B](xs: List[A], zero: B, f: (B, A) ⇒ B): B = {
        xs.foldLeft(zero)(f)
      }
    }
  }

  def foldSum[M[_] : FoldLeft, A: Monoid](data: M[A]): A = {
    val monoid = implicitly[Monoid[A]]
    FoldLeft[M].foldLeft(data, monoid.zero, monoid.mappend)
  }

  println(s"foldSum: ${foldSum(data)}")

  def plus[A: Monoid](a: A, b: A): A = implicitly[Monoid[A]].mappend(a, b)

  println(s"plus ${plus(12, 3)}")


  trait MonoidOp[A] {
    val F: Monoid[A]
    val value: A
    def |+|(a2: A) = F.mappend(value, a2)
  }


  implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
    val F = implicitly[Monoid[A]]
    val value = a
  }

  println(2 |+| 3)
}
