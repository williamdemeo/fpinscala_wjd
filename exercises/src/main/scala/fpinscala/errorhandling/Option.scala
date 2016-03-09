package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  
  // map := take f:A=>B and a:Option[A] and return
  // None if not Some(a), otherwise return Some(f(a))
  // (Use this when f is total (defined for all a:A).)
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a)) 
  }

  // getOrElse := take d:B and a:Option[A] and return
  // d if not Some(a), otherwise return a.
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  // flatMap := take f:A=>Option[B] and a:Option[A] and return
  // None if not Some(a), otherwise return Some(f(a)) 
  def flatMap[B](f: A => Option[B]): Option[B] =  map(f) getOrElse(None)
  // N.B. this is a "flattening" map only in the sense that map takes f:A=>B, 
  // whereas flatMap takes f:A=>Option[B]; both return type Option[B].
  // If we had tried to define flatMap as simply map(f), we get a type error
  // because then the return type would be Option[Option[B]].
  
  // we can also implement `flatMap` with explicit pattern matching 
  // (which seems much clearer to me).
  def flatMap_pm[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  
  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse(ob) 

  def orElse_pm[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }
  
  // filter := return Some(a) if f(a) true, else return None
  def filter_pm(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) Some(a) else None
    case _ => None 
  }
  // pm provides, as usual, the implementation that is easiest to interpret
  
  // we could have defined filter in terms of map
  def filter_with_map(f: A => Boolean): Option[A] = if (this map(f) getOrElse(false)) this else None 

  // or in terms of flatmap
  def filter_with_flatmap(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = sys.error("todo")

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = sys.error("todo")

  def sequence[A](a: List[Option[A]]): Option[List[A]] = sys.error("todo")

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}