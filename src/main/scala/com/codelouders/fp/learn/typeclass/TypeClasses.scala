package com.codelouders.fp.learn.typeclass

/**
  * Created by Wiktor on 27/04/2017.
  */

/**
  * Semigroup
  * laws:
  *   associative: x + (y + Z) = (x + y) + z
  *
  */
trait Semigroup[A] {
  def combine(a1: A, a2: A): A
}

  /**
  * Monoid
  * laws:
  *   identity: empty + x = x + empty = x
  *   associative: x + (y + Z) = (x + y) + z
  *
  */
trait Monoid[A] extends Semigroup[A] {
  def empty: A        //mempty
  // combine is defined in semigroup
  // def combine(a1: A, a2: A): A
}

object Monoid {
  def apply[A: Monoid]: Monoid[A] = implicitly

  implicit val intMonoid = new Monoid[Int] {
    override def empty: Int = 0
    override def combine(a1: Int, a2: Int): Int = a1 + a2
  }

  implicit val stringMonoid = new Monoid[String] {
    override def empty: String = ""
    override def combine(a1: String, a2: String): String = a1 + a2
  }
}


trait FoldLeft[F[_]] {
  def foldleft[A, B](xs: F[A], zero: B)(f: (B, A) ⇒ B): B
}

object FoldLeft {

  def apply[M[_]: FoldLeft]: FoldLeft[M] = implicitly
  
  implicit val listFold = new FoldLeft[List] {
    override def foldleft[A, B](xs: List[A], zero: B)(f: (B, A) ⇒ B): B = xs.foldLeft(zero)(f)
  }
}

trait Functor[F[_]] {
  def map[A, B](xs: F[A], f: A ⇒ B): F[B]
}

object Functor {

  def apply[M[_]: Functor]: Functor[M] = implicitly

  implicit val listFunctor = new Functor[List] {
    override def map[A, B](xs: List[A], f: (A) ⇒ B): List[B] = xs.map(f)
  }
}

object TestTypeClasses extends App {

  def combine[M[_]: FoldLeft, A: Monoid](xs: M[A]): A = {
    FoldLeft[M].foldleft(xs, Monoid[A].empty)(Monoid[A].combine)
  }

  println(combine(List(1, 2 ,3, 4)))

  def eachToString[M[_]: Functor, A](xs: M[A]): M[String] = {
    Functor[M].map[A, String](xs, t ⇒ t.toString)
  }

  println(combine(eachToString(List(1, 2 ,3, 4))))
  println(combine(eachToString(List(1.0, 2.0 ,3.0, 4.0))))

}