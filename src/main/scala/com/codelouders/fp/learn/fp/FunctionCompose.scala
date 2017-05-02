package com.codelouders.fp.learn.fp

/**
  * Created by Wiktor on 30/04/2017.
  */
object FunctionCompose extends App {

  val intToPrettyString: (Int) ⇒ String = (value) ⇒ s"value is $value"
  val boolToPrettyString: (Boolean) ⇒ String = (value) ⇒ s"value is $value"


  val add: (Int) ⇒ Int = (a) ⇒ a * a

  val compose = intToPrettyString compose add
  println(compose(5))

  val andThen = add andThen intToPrettyString
  println(andThen(5))

  val multiply: (Int, Int) ⇒ Int = (a, b) ⇒ a * b

  def multiplicationToNiceString(i: Int): (Int) ⇒ String = intToPrettyString compose multiply.curried(i)
  println(multiplicationToNiceString(3)(2))

  val lessThan10: PartialFunction[Int, Boolean] = {
    case a: Int if a < 10 ⇒
      true
  }

  val rest: PartialFunction[Int, Boolean] = {
    case _: Int ⇒ false
  }

  val slittedLogic = lessThan10 orElse rest andThen boolToPrettyString

  println(slittedLogic(7))
  println(slittedLogic(12))
}
