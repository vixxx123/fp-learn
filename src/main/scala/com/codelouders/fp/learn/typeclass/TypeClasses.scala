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


  implicit def optionMonoid[A: Monoid]  = new Monoid[Option[A]] {
    override def empty: Option[A] = None
    override def combine(a1: Option[A], a2: Option[A]): Option[A] = {
      a1 match {
        case None => a2
        case Some(a1v) =>
          a2 match {
            case None => a1
            case Some(a2v) => Some(Monoid[A].combine(a1v, a2v))
          }
      }
    }
  }

  implicit def mapMonoid[K, V: Monoid] = new Monoid[Map[K, V]] {
    override def empty: Map[K, V] = Map.empty
    override def combine(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = {
      a1 ++ a2.map{ case (k,v) => k -> Monoid[V].combine(v, a1.getOrElse(k, Monoid[V].empty)) }
    }
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def combine(x: A, y: A): A = m.combine(y, x)
    override val empty: A = m.empty
  }

  // exercise 10.3
  implicit def endoMonoid[A]: Monoid[A => A] = new Monoid[A ⇒ A] {
    override def empty: (A) ⇒ A = x ⇒ x
    override def combine(f1: (A) ⇒ A, f2: (A) ⇒ A): (A) ⇒ A = f1 andThen f2
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
  println(combine(List(Option(3), Option(5), None)))
  println(combine(List(Map("a" → 1, "c" → 10), Map("a" → 3, "b" → 2), Map("a" → 4, "b" → 2))))

  def eachToString[M[_]: Functor, A](xs: M[A]): M[String] = {
    Functor[M].map[A, String](xs, t ⇒ t.toString)
  }

  println(combine(eachToString(List(1, 2 ,3, 4))))
  println(combine(eachToString(List(1.0, 2.0 ,3.0, 4.0))))

  // exercise 10.6
  def foldMap[A, B: Monoid](as: List[A])(f: A ⇒ B): B = {
    as.foldLeft(Monoid[B].empty)((b, a) ⇒ Monoid[B].combine(b, f(a)))
  }

  def foldMap2[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))


//  def foldLeft[A, B: Monoid](xs: List[A]): B = {
//    foldMap(xs)({ a:A ⇒ b:B ⇒ Monoid[B].combine(b, a)})(Monoid.dual(Monoid.endoMonoid[B]))(Monoid[B].empty)
//  }

  def foldLeft2[A, B: Monoid](xs: List[A])(f: (B, A) => B): B = {
    val comb: (A) ⇒ (B) ⇒ B = { a: A ⇒ b:B ⇒ f(b, a)}
    foldMap(xs)(comb)(Monoid.endoMonoid[B])(Monoid[B].empty)
  }

  def foldLeft3[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap2(as, Monoid.endoMonoid[B])(a => b => f(b, a))(z)

  val list = List(1, 2, 3)
//  println(foldLeft(list)())
  println(foldLeft2(list)((b: String, a) ⇒ b + a))

  println(foldLeft3(list)("")((b: String, a) ⇒ b + a))

  println(list.foldLeft("")((b, a) ⇒ b + a))
}