/* File: Stream.scala (Ch 5)
 * Authors: Paul Chiusano and Runar Bjarnason
 * Url: https://github.com/fpinscala/fpinscala 
 * 
 * Description: This is a modified version of the file Stream.scala
 *   that accompanies the book "Functional Programming in Scala" by
 *   Chiusano and Bjarnason. This version of the file includes 
 *   solutions to some of the exercises in 
 * 
 *     CHAPTER 5: Strictness and laziness
 * 
 *   The solutions herein are by William DeMeo <williamdemeo@gmail.com>.
 *   They are at best imperfect, and possibly wrong.  Official solutions by 
 *   Chiusano and Bjarnason are available in the github repo mentioned above.
 */

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
    case Cons(h,_) => Some(h())
  }
  def headOption: Option[A] = foldRight(None: Option[A])((h,_) => Some(h))
  // copied from official solution


  // Ex 5.7: Implement map, filter, append, and flatMap using foldRight. 
  //         The append method should be non-strict in its argument.
  //         (Part of the exercise is writing your own function signatures.)
  //
  // map (pm version)
  def map_pm[B](f: A => B):Stream[B] = this match {
    case Cons(h,t) => cons( f(h()), t().map_pm(f) )
    case _ => empty[B]
  }
  //  
  // map (with foldRight)
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((x,y) => cons(f(x),y))
  // checked (same as official solution)
  
  // filter (pm version)
  def filter_pm(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) => if( p(h()) ) cons( h(), t().filter_pm(p) ) else t().filter_pm(p)
    case _ => empty[A]
  }
  
  // filter (with foldRight)
  def filter(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])( (x,y) => if(p(x)) cons(x,y) else y ) 
  // checked (same as official solution)
    
  // append (pm version)
  def append_pm[B>:A](s: Stream[B]): Stream[B] = this match {
    case Empty => s
    case Cons(h,t) => cons(h(), t().append_pm(s)) 
  }
  
  // append (with foldRight)
  def append[B>:A](s: Stream[B]): Stream[B] = foldRight(s)((x,y) => cons(x,y))
  // checked (same as official solution)
  
  // flatMap (pm version)
  def flatMap_pm[B>:A](f: A => Stream[B]): Stream[B] = this match {
    case Cons(h,t) => f(h()) append t().flatMap(f)
    case _ => Empty
  }
  
  // flatMap (with foldRight)
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((x,y) => f(x) append y)
  // checked (same as official solution)
      

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  
  // find (with filter, i.e., foldRight)
  final def find_with_filter(p: A => Boolean): Option[A] = filter(p).headOption
  // NB filter transforms the whole stream but it does so lazily, 
  // so find terminates as soon as a match is found.
  
  // (Exercises 5.8--5.12 are solved below in the Stream object.)
  // Ex 5.13 Use unfold to implement map, take, takeWhile, zipWith (see ch 3), and zipAll. 
  // Recall, unfold's signature,
  //
  //    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]
  //
  // It takes an initial state z and a function that knows how to convert a state into
  // Some(value, next state) or None.
  //
  //---- map (via unfold) ----------------------------------------
  def map_with_unfold_first_try[B](f: A => B): Stream[B] = 
    unfold(this) ( z => z match {
      case Cons(h,t) => Some( ( f(h()), t() ) ) 
      case _ => None // converted to empty stream by unfold.
    })
  // This is similar to official solution, but note that 
  //  z => z match { ... }  is unnecessary.
  // We can simply insert the cases and the function will be defined, 
  // as follows:
  def map_with_unfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h,t) => Some( ( f(h()), t() ) ) 
    case _ => None // converted to empty stream by unfold.
  }
  //    
  //---- take (via unfold) -------------------------------------------
  def take_with_unfold(n: Int): Stream[A] = unfold((this,n)) {
    case (Cons(h, t), 1) => Some((h(), (empty, 0)))
    case (Cons(h, t), n) if (n>0) => Some((h(), (t(), n-1))) 
  	case _ => None // converted to empty by unfold.
	}
  // checked (now correct; it was almost right, but I left off the first case)
  //
  //---- takeWhile (via unfold) -------------------------------------------
  def takeWhile_with_unfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h,t) if p(h()) => Some((h(),t()))
    case _ => None // converted to empty stream by unfold.
  }

  // Recall original zipWith function from Chapter 3 (Exercise 3.23): 
  // It takes two lists and a binary function and returns the list consisting of 
  // the result of applying the function to each pair of elements from the lists.
  // Here's the signature:
  //    def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C]
  //
  //---- zipWith (via unfold) ---------------------------------------------
  def zipWith[B,C](b: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this,b)) { 
    case ( Cons(ah,at), Cons(bh,bt) ) => Some( ( f(ah(),bh()), (at(), bt()) ) )
    case _ => None
  }
  // checked (same as official solution)
  
  //---- zipAll (with unfold) ---------------------------------------------- 
  // The zipAll function should continue the traversal as long as either stream has more 
  // elements--it uses Option to indicate whether each stream has been exhausted.
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this,s2)) { 
    case ( Cons(ah,at), Cons(bh,bt) ) => Some( (Some(ah()), Some(bh())), ( at(),  bt()) )
    case ( Cons(ah,at),         _   ) => Some( (Some(ah()),     None  ), ( at(), empty) )
    case (         _  , Cons(bh,bt) ) => Some( (     None , Some(bh())), (empty,  bt()) )
    case _ => None
  }
  
  // Ex 5.14 (hard) Implement startsWith using functions you've written. 
  // It should check if one Stream is a prefix of another. For instance, 
  // Stream(1,2,3) startsWith Stream(1,2) would be true.
  //
  def startsWith_first_try[B](s: Stream[B]): Boolean = zipWith(s)(_ == _).foldRight(true)(_ && _)
  // This doesn't work when `this` ends before s.
  //
  def startsWith_second_try[B](s: Stream[B]): Boolean =	zipAll(s).map{
  		case (Some(x), Some(y)) => x==y
  		case (_, None) => true
  		case _ => false
  	}.foldRight(true)(_ && _)
  // This works, but it's not as pretty as the official solution below.
 	//
	def startsWith[A](s: Stream[A]): Boolean = 
		zipAll(s).takeWhile(!_._2.isEmpty) forAll { case (h,h2) => h == h2 }

  // Ex 5.15 Implement tails using unfold. For a given Stream, tails returns the 
  // Stream of suffixes of the input sequence, starting with the original Stream. 
  // For example, given Stream(1,2,3), it would return 
  // Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()) .
  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h,t) => Some( (Cons(h,t), t()) )
  	case _ => None
  } append Stream(empty)

  // Implement hasSubsequence using startsWith and tails.  NB we are not asking 
  // if s2 is a true subsequence of `this` in the mathematical sense. That's harder.  
  // Instead, we simply want to check if s2 appears as a *contiguous* substring 
  // of `this`.  (Calling this a "subsequence" seems misleading to a mathematician.)
  def hasSubsequence[A](s2: Stream[A]): Boolean = tails exists (_ startsWith(s2))
  // For example, in this implementation, 
  //     Stream(1,2,3) hasSubsequence(Stream(1,3)) 
  // returns false, even though (1,3) is a subsequence of (1,2,3).
  
  // Ex 5.16 (hard) Generalize tails to the function scanRight, which is like a 
  // foldRight that returns a stream of the intermediate results. For example:
  // 
  //     scala> Stream(1,2,3).scanRight(0)(_ + _).toList
  //     res0: List[Int] = List(6,5,3,0)
  //
  // This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0, 0). 
  // Your function should reuse intermediate results so that traversing a Stream with n
  // elements always takes time linear in n. Can it be implemented using unfold? How, or
  // why not? Could it be implemented using another function we've written?
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = tails.map(_.foldRight(z)(f))
  // This solution seems to work, but it probably doesn't satisfy the requirement that
  // the stream is traversed only once.
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

  // Ex 5.8 Generalize ones slightly to the function constant, 
  // which returns an infinite Stream of a given value.
  def constant_first_try[A](a: A): Stream[A] = cons(a,constant_first_try(a))

  // better version (copied official solution):
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // Ex 5.9 Write a function that generates an infinite stream of integers, 
  // starting from n, then n + 1, n + 2, and so on.
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  // checked (same as official solution)

  // Ex 5.10 Write a function fibs that generates the infinite stream of 
  // Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  def fibs: Stream[Int] = {
    def fibs_aux(n0: Int, n1: Int): Stream[Int] = cons(n0, fibs_aux(n1, n0+n1)) 
    fibs_aux(0,1)    
  }

  // The following doesn't seem to work. (Maybe it isn't non-strict?)
  def fibs_first_try: Stream[Int] = {
    def fibs_aux(n0: Int, n1: Int, acc: Stream[Int]): Stream[Int] = 
      fibs_aux(n1, n0+n1, acc append cons(n0, cons(n1, empty))) 
    fibs_aux(0,1,empty)    
  }

  // Ex 5.11 Write a more general stream-building function called unfold. 
  // It takes an initial state, and a function for producing both the next 
  // state and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case None => empty 
    }
  // checked (same as official solution)
    
  // Ex 5.12 Write fibs, from, constant, and ones in terms of unfold.
  //
  // fibs (with unfold)
  def fibs_with_unfold: Stream[Int] = 
    unfold((0,1))( s => Some( ( s._1, (s._2, s._1+s._2) ) ) ) 
  // checked (it works and is similar to official solution)
  //
  // from (with unfold)
  def from_with_unfold(n: Int): Stream[Int] = unfold(n)( s => Some( (s, s+1) ) )
  // checked (same as official solution)
  //
  // constant (with unfold)
  def constant_with_unfold[A](a: A): Stream[A] = unfold(a)( _ => Some( (a,a) ) )
  // checked (same to official solution)

  // ones (with unfold)
  def ones_with_unfold = constant_with_unfold(1)
  // checked (works)
  /*
   __Notes about unfold versions__ 
   Using unfold to define `constant` and `ones` means that we don’t get sharing 
   as in the recursive definition `val ones: Stream[Int] = cons(1, ones)`. 
   The recursive definition consumes constant memory even if we keep a reference 
   to it around while traversing it, while the unfold-based implementation does not. 
   Preserving sharing isn't something we usually rely on when programming with streams, 
   since it's extremely delicate and not tracked by the types. For instance, sharing is 
   destroyed when calling even `xs.map(x => x)`.
	*/
  
  
  
    
    
    
    
    
}