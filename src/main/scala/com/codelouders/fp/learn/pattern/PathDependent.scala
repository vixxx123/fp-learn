package com.codelouders.fp.learn.pattern

/**
  * Created by Wiktor on 27/04/2017.
  */
object PathDependent extends App {
  class Parent {
    class Child
    var child: Option[Child] = None
  }

  val p1 = new Parent
  val p2 = new Parent
  val p1c = new p1.Child
  val p2c = new p2.Child
  p1.child = Some(p1c)
  p2.child = Some(p2c)

  // This will not compile!!!!!
  // p1.child = Some(p2c)
}
