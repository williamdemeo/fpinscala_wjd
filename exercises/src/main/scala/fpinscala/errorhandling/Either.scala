package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {

  // Here are the pm versions:
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
  	this match {
    	case Left(e) => Left(e)
    	case Right(a) => b match {
      	case Left(e) => Left(e)
      	case Right(b) => Right(f(a,b))
    	}
  	}

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
	def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
  	sequence(es map (f))
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match {
    case Nil => Right(Nil)
    case h::t => h flatMap (hh => sequence(t) map(hh::_))
  }
  // alternative sequence implementation
  def sequence_second_try[E,A](es: List[Either[E,A]]): Either[E,List[A]] = 
    es.foldRight[Either[E,List[A]]](Right(Nil))((x,y) => x.map2(y)(_::_))

    
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