// Authors: Paul Chiusano and Runar Bjarnason
// Url: https://github.com/fpinscala/fpinscala 
// 
// Exercises in this file were solved by williamdemeo@gmail.com.
// The solutions may be imperfect. Authoritative solutions are provided by 
// Chiusano and Bjarnason at the github repository cited above.

package fpinscala.laziness

import Stream._
trait Stream[+A] {

	// Ex 5.1: Write a function to convert a Stream to a List, which will 
	//         force its evaluation and let you look at it in the REPL. 
	//         You can convert to the regular List type in the standard library. 
	def toList: List[A] = this match {
  	case Empty => Nil
  	case Cons(h, t) => h() :: t().toList
  }
  
  // Ex 5.2: Write the function take(n) for returning the first n elements of  
  //         a Stream, and drop(n) for skipping the first n elements of a Stream.
  // 5.2a
  def take(n: Int): Stream[A] = (this, n) match {
  	case (Empty, _) => Empty
  	case (_, 0) => Empty
    case (Cons(h, t), m) => cons(h(), t().take(m-1))
	}
  // 5.2b
  def drop(n: Int): Stream[A] = (this, n) match {
  	case (Empty, _) => Empty
  	case (_, 0) => this
  	case (Cons(h, t), m) => t().drop(m-1) 
  }

  // Ex 5.3: Write the function takeWhile for returning all *starting* elements 
  //         of a Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = this match {
  	case Empty => Empty
  	case Cons(h,t) => if(p(h())) cons( h(), t() takeWhile(p) ) else Empty
  }

  // My first implementation of takeWhile was wrong because I misread the spec.
  // It would be more accurately described as `takeIf`, defined as follows:
  def takeIf(p: A => Boolean): Stream[A] = this match {
  	case Empty => Empty
  	case Cons(h,t) => 
  		if(p (h()) ) cons(h(), t() takeIf(p)) else t() takeIf(p)	
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = 
  	// The arrow `=>` in front of the argument type `B` means that the function `f` 
  	// takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) 
                        // If `f` doesn't evaluate its second argument, 
                        // the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b) 
    // Here `b` is the unevaluated recursive step that folds the tail of the stream. 
    // If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

	// Ex 5.4: Implement forAll, which checks that all elements in the Stream 
  //         match a given predicate. Your implementation should terminate the 
  //         traversal as soon as it encounters a non-matching value.
  def forAll_first_try(p: A => Boolean): Boolean = this match {
  	case Empty => true
  	case Cons(h, t) => if (p(h())) t() forAll(p) else false
  }
  def forAll(p: A => Boolean): Boolean = this match {
  	case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  // Ex 5.5: Use foldRight to implement takeWhile.
  def forAll_with_fold(p: A => Boolean): Boolean = sys.error("todo")

  // Ex 5.6: (Hard) Implement headOption using foldRight.
  def headOption: Option[A] = sys.error("todo")


  // Ex 5.7: Implement map, filter, append, and flatMap using foldRight. 
  //         The append method should be non-strict in its argument.
  //         (Part of the exercise is writing your own function signatures.)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}