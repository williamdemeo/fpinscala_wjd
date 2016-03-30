/* File: Option.scala (Ch 4)
 * Authors: Paul Chiusano and Runar Bjarnason
 * Url: https://github.com/fpinscala/fpinscala 
 * 
 * Description: This is a modified version of the file Option.scala
 *   that accompanies the book "Functional Programming in Scala" by
 *   Chiusano and Bjarnason. This version of the file includes 
 *   solutions to some of the exercises in 
 * 
 *     CHAPTER 4: Handling errors without exceptions
 * 
 *   The solutions herein are by William DeMeo <williamdemeo@gmail.com>.
 *   They are at best imperfect, and possibly wrong.  Official solutions by 
 *   Chiusano and Bjarnason are available in the github repo mentioned above.
 */

package fpinscala.errorhandling

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Some => _, Either => _, _} 

sealed trait Option[+A] {
  
  // map: Type B => Option[A] => (A => B) => Option[B]
  // map[B] takes an Option[A] and a A => B and returns
  //        -- None if not Some(a), 
  //        -- Some(f(a)) otherwise
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a)) 
    case _ => None
  }
  // N.B. the Option[A] argument type appears because map is applied to `this`
  //  
  // We use the above map function when f is total... 
  // ...so, why does map return Option[B]? Because,
  // the thing to which it is applied might come back empty.
  // Example: lookupByNmae("Joe").map(_.department)
  // Here, lookupByName("Joe") could fail.  If it succeeds, then
  // the _.department function works as expected (it's total).

  // getOrElse : Type B => Option[A] => B 
  // getOrElse[B] takes an Option[A] and returns
  //       -- a if Some(a),
  //       -- d otherwise.
  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }
  // IMPORTANT: the type annotation `default: => B` means that the
  // argument named `default` is passed using call-by-name evaluation. 
  // This means it will not be evaluated unless/until needed.

  // flatMap: Type B => Option[A] => (A => Option[B]) => Option[B]
  // flatMap[B] takes an Option[A] and a A => Option[B] and returns
  //        -- None if not Some(a), 
  //        -- f(a) otherwise
  // we first try the simpler pm implementation:
  def flatMap_pm[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case _ => None
  }

  // now for the implementation that uses map and getOrElse:
  def flatMap[B](f: A => Option[B]): Option[B] =  map(f) getOrElse(None)
  // Notice that `f: A => Option[B]`. Therefore, if x:Option[A], then
  // x map(f): Option[Option[B]]. We must unwrap this result one level in order to
  // return an Option[B].  This is a "flattening" because it composes two things, 
  // both of which can fail---the "this" and the function evaluation f(Some(a))---and
  // instead of returning an Option[Option[B]], we return an Option[B] (hence, flattening).
  
  // orElse: Type B => Option[A] => Option[B] => Option[B]
  // orElse[B] takes this a: Option[A] and ob:Option[B] and returns
  //      -- Some(a) if Some(a)
  //      -- ob otherwise
  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse(ob) 

  def orElse_pm[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case _ => ob
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

  def variance(xs: Seq[Double]): Option[Double] =
       mean(xs).flatMap (m => mean(xs.map(x => math.pow(x-m,2))))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map(f)

  // worked on this for a while, trying to be fancy, then realized a 
  // simple pm is a nice and easy solution. 
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
    case (Some(a), Some(b)) => Some(f(a,b))
    case (_,_) => None
  }
  
  // Here's the slick (obfuscated) way to do it:
  def map2_second_try[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa,bb)))
    
  // Explanation: map has type signature `Option[A] => (A => B) => Option[B]`
  //    the function f: (A,B) => C is ordinary (total) function, but 
  //    arguments passed to it in this example may be None's.
  // OTOH, flatMap has type signature `Option[A] => (A => Option[B]) => Option[B]`
  // We need flatMap here because the return type of the function
  //     aa => b map (bb => f(aa,bb)) 
  // is already Option[C]
  
  // Here's a modified lifting function that works for binary functions.
  def lift2[A,B,C](f: (A,B) => C) : (Option[A], Option[B]) => Option[C] = { 
    (a,b) => a flatMap (aa => b map (bb => f(aa,bb)))
  }
  // lift2 can be used to give yet another version of map2 
  // (this merely abstracts the essence of the second_try solution above)    

  //def map2_third_try[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = lift3(f)(a,b)
  def map2_third_try[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C) = lift2(f)(a,b)

  // This one is harder (had to peek at answers)
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h::t => h flatMap (hh => sequence(t) map (hh::_))
  }

  // The answers suggest it can be done with foldRight and map2. Let's try it.
  def sequence_second_try[A](a: List[Option[A]]): Option[List[A]] = 
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)( _::_ ))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => f(h) match {
      case None => None
      case Some(y) => Some(y) flatMap (y => traverse(t)(f) map (y::_))
    }
  }

  // cleaner version:
  def traverse_second_try[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h),traverse_second_try(t)(f))(_ :: _)
  }
  
  // book's alternative answer:
  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))


    
}