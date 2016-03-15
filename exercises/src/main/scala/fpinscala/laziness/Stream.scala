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
	def toList_first_try: List[A] = this match {
  	case Empty => Nil
  	case Cons(h, t) => h() :: t().toList_first_try
  }
  
	// As mentioned in the book, the first try above will stack overflow for large 
	// streams.  Here is a tail-recursive version.
	def toList: List[A] = {
	  @annotation.tailrec
	  def toList_tailrec(s: Stream[A], acc: List[A]): List[A] = s match {
	    case Empty => acc
	    case Cons(h, t) => toList_tailrec(t(), h()::acc)
	  }
	  toList_tailrec(this, Nil).reverse
	}
	// We could avoid the need for reverse by using a mutable list buffer. (See solutions.)
	
	
  // Ex 5.2: Write the function take(n) for returning the first n elements of  
  //         a Stream, and drop(n) for skipping the first n elements of a Stream.
  // 5.2a
  def take(n: Int): Stream[A] = (this, n) match {
    case (Cons(h, t), m) if m > 1 => cons(h(), t().take(m-1))
  	case (Cons(h, _), 1) => cons(h(), Empty)
  	case _ => Empty
	}
  
 
  
  // 5.2b: (If we declare a public function tailrec, we must mark it `final`
  //       so it can't be overwritten.)
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
  	case Cons(h, t) if n > 0 => t().drop(n-1) 
  	case _ => this
  }

  
  // Ex 5.3: Write the function takeWhile for returning all *starting* elements 
  //         of a Stream that match the given predicate.
  def takeWhile(p: A => Boolean): Stream[A] = this match {
  	case Cons(h,t) if(p(h())) => cons( h(), t() takeWhile(p) )
  	case _ => Empty
  }

  
  // My first implementation of takeWhile was wrong because I misread the spec.
  // It would be more accurately described as `takeIf`, defined as follows:
  def takeIf(p: A => Boolean): Stream[A] = this match {
  	case Cons(h,t) => if(p (h()) ) cons(h(), t() takeIf(p)) else t() takeIf(p)
  	case _ => Empty
  }

  
  // exists with explicit recursion (p. 70) 
  def exists_first_try(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists_first_try(p)
    case _ => false
  }
  
  // exists with general recursion (using foldRight)
  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b) 
    // Here `b` is the unevaluated recursive step that folds the tail of the stream. 
    // If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  
	// Ex 5.4: Implement forAll, which checks that all elements in the Stream 
  //         match a given predicate. Your implementation should terminate the 
  //         traversal as soon as it encounters a non-matching value.
  // forAll with explicit recursion (first try)
  def forAll_first_try(p: A => Boolean): Boolean = this match {
  	case Empty => true
  	case Cons(h, t) => if (p(h())) t() forAll(p) else false
  }
  // forAll with explicit recursion (second try)
  def forAll_second_try(p: A => Boolean): Boolean = this match {
  	case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }
  // forAll with general recursion (using foldRight)
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)
    
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match { 
  	// The arrow `=>` in front of the argument type `B` means that the function `f` 
  	// takes its second argument by name and may choose not to evaluate it.
    case Cons(h,t) => f(h(), t().foldRight(z)(f)) 
    // If `f` doesn't evaluate its second argument, the recursion never occurs.
    case _ => z
  }
 
  
  // Ex 5.5: Use foldRight to implement takeWhile.
  def takeWhile_with_foldRight(p: A => Boolean): Stream[A] = 
    foldRight[Stream[A]](Empty)( (a,b) => {if(p(a)) cons(a, b) else Empty} )

    
  // Ex 5.6: (Hard) Implement headOption using foldRight.
  def headOption_first_try: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }
  def headOption: Option[A] = foldRight(None: Option[A])((h,_) => Some(h))


  // Ex 5.7: Implement map, filter, append, and flatMap using foldRight. 
  //         The append method should be non-strict in its argument.
  //         (Part of the exercise is writing your own function signatures.)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
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