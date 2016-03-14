// Authors: Paul Chiusano and Runar Bjarnason
// Url: https://github.com/fpinscala/fpinscala 
// 
// Exercises in this file were solved by williamdemeo@gmail.com.
// The solutions may be imperfect. Authoritative solutions are provided by 
// Chiusano and Bjarnason at the github repository cited above.

package fpinscala.errorhandling

// Hide std library `Option` and `Either`, 
// since we are writing our own in this chapter
// NB: in answers they leave `Left => _, Right => _` out of the mask.
import scala.{Option => _, Either => _, Left => _, Right => _, _} 

sealed trait Either[+E,+A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  } // correct: (same as answers)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  } // correct: (same as answers)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  } // correct: (same as answers)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
  	this match {
    	case Left(e) => Left(e)
    	case Right(a) => b match {
      	case Left(e) => Left(e)
      	case Right(b) => Right(f(a,b))
    	}
  	} // this may be correct, but in answers they use a for-comprehension
		  // as follows:

  def map2_with_for[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): 
  	Either[EE, C] = 
  		for { 
  			aa <- this 
  			bb <- b 
  		} yield f(aa, bb)
  	
	  // The for-comprehension is explained on page 60 of fpinscala.
  	// It is syntactic sugar for multiple flatMaps and one map. 
  	// If there are n expressions of the form `a <-` the compiler 
  	// desugars the for-comprehension to n-1 flatMaps and one final map.
  	// For example, the function map2_with_for above becomes 
  def map2_desugared[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
  	this flatMap (aa =>	
  	b	map (bb => 
  	f(aa,bb) ))
  	
  // Why no getOrElse this time?  Maybe it's less useful in the Either 
  // trait than it is in the Option trait.
  def getOrElse[EE >: E, B >: A](b: => B): B = this match {
    case Left(e) => b
    case Right(a) => a
  }
  
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

	// I defined sequence first and then defined traverse in terms of sequence
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match {
    case Nil => Right(Nil)
    case h::t => h flatMap (hh => sequence(t) map(hh::_))
  }
	// alternative sequence implementation
  def sequence_second_try[E,A](es: List[Either[E,A]]): Either[E,List[A]] = 
    es.foldRight[Either[E,List[A]]](Right(Nil))((x,y) => x.map2(y)(_::_))

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): 
		Either[E, List[B]] = sequence(es map (f))


    
  // The book defines traverse first, and defines sequence via traverse:
  def traverse_book_version[E,A,B](es: List[A])(f: A => Either[E,B]): Either[E, List[B]] = 
  	es match {
  		case Nil => Right(Nil)
  		case h::t => (f(h) map2 traverse_book_version(t)(f))(_ :: _)
  }
  
  def sequence_book_version[E,A](es: List[Either[E,A]]): Either[E,List[A]] = 
  	traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}