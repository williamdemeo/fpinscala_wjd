/* File: Monoid.scala (Ch 10)
 * Authors: Paul Chiusano and Runar Bjarnason
 * Url: https://github.com/fpinscala/fpinscala 
 * 
 * Description: This is a modified version of the file Monoid.scala
 *   that accompanies the book "Functional Programming in Scala" by
 *   Chiusano and Bjarnason. This version of the file includes 
 *   solutions to some of the exercises in 
 * 
 *     CHAPTER 10: Monoids
 * 
 *   The solutions herein are by William DeMeo <williamdemeo@gmail.com>.
 *   They are at best imperfect, and possibly wrong.  Official solutions by 
 *   Chiusano and Bjarnason are available in the github repo mentioned above.
 */
package fpinscala.monoids

import fpinscala.testing._
import Prop._
import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // Ex 10.1 Give Monoid instances for integer add and multiply as well as Boolean operators.
  val intAdditive = new Monoid[Int] {
  	def op(a1: Int, a2: Int) = a1+a2
  	val zero = 0
  }

  val intMultiplicative = new Monoid[Int] {
  	def op(a1: Int, a2: Int) = a1*a2
  	val zero = 1
  }

  val booleanOr = new Monoid[Boolean]{
  	def op(a1: Boolean, a2: Boolean) = a1 || a2
  	val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
  	def op(a1: Boolean, a2: Boolean) = a1 && a2
  	val zero = true
  }

  // Ex 10.2 Give a Monoid instance for combining Option values.
  def optionMonoid_first_try[A] = new Monoid[Option[A]] {
  	def op(a1: Option[A], a2: Option[A]) = (a1,a2) match {
  		case (Some(a), _) => Some(a)
  		case (None, Some(b)) => Some(b)
  		case (None, None) => None
  	}   // This makes clear what op does, but a much more concise version 
      	// uses orElse (see alternative below).
  	val zero = None 
  }
  // Use the orElse combinator of the Option trait:
  def optionMonoid[A] = new Monoid[Option[A]] {
  	def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
  	val zero = None 
  }

  // Ex 10.3 A function having the same argument and return type is sometimes called an endofunction. 
  // Write a monoid for endofunctions.
  def endoMonoid[A] = new Monoid[A => A] {
  	def op(f: A => A, g: A => A) = f compose g // i.e., x => f(g(x))
  	def zero = {a => a}
  }

	// Ex 10.4 Use the property-based testing framework we developed in part 2 to implement a
	// property for the monoid laws. Use your property to test the monoids we've written.	
  def monoidLawsPredicate[A](m: Monoid[A])(t: (A,A,A)): Boolean =	t match { 
  	case (a,b,c) => {
  		(m.op(m.op(a, b),c) == m.op(a, m.op(b,c))) && // associative
  		(m.op(a, m.zero)== a) && (m.op(m.zero,a)==a)  // identity
		}
  }
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  	forAll(Gen.triple(gen))(monoidLawsPredicate(m))

  // Second try (I think the previous one is clearer)
  def monoidLaws_alt[A](m: Monoid[A], gen: Gen[A]): Prop =
  	// associative law
  	forAll(Gen.triple(gen))(t => 
  		(m.op(m.op(t._1, t._2),t._3) == m.op(t._1, m.op(t._2, t._3)))) &&
 		// identity law
  	forAll(gen)(a => (m.op(a, m.zero)== a) && (m.op(m.zero,a)==a))  

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    sys.error("todo")

	// Ex 10.5 Implement foldMap.
	def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = (as.map(f)).foldRight(m.zero)(m.op)
  
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

	// Ex 10.6 (Hard) foldMap can be implemented using either foldLeft or foldRight. 
  // But you can also write foldLeft and foldRight using foldMap! Try it.  
  def foldRight_via_foldMap[A,B](as: List[A])(z: B)(f: (A,B) => B): B =
	  foldMap(as, endoMonoid[B])(a => (b => f(a,b)))(z)

	// Ex 10.7 Implement a foldMap for IndexedSeq. Your implementation should use the strategy
	// of splitting the sequence in two, recursively processing each half, and then adding the
	// answers together with the monoid.
	def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = 
		if (v.size==0) m.zero
		else if (v.size==1) f(v(0))
		else {
			val (l,r)=v.splitAt(v.length/2)
			m.op(foldMapV(l, m)(f), foldMapV(r,m)(f))
		}

	// Ex 10.8 (Hard) Implement a parallel version of foldMap using the library from ch 7. 
	// Hint: Implement par, a combinator to promote Monoid[A] to a Monoid[Par[A]], and then 
	// use this to implement parFoldMap.
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
  	def op(a1: Par[A], a2: Par[A]) = Par.map2(a1,a2)(m.op)
  	def zero = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
	  Par.flatMap(Par.parMap(v)(f)){ bs => 
			foldMapV(bs,par(m))(b => Par.lazyUnit(b))
		}

  // Ex 10.9 (Hard) Use foldMap to detect whether a given IndexedSeq[Int] is ordered. 
  // You'll need to come up with a creative Monoid.
  // Hint: construct a type that keeps track of the _interval_ of values of a given segment
  // as well as whether an unordered segment has been found.
  def ordered(ints: IndexedSeq[Int]): Boolean = {
  	val m = new Monoid[Option[(Int,Int,Boolean)]] {
  		def op(a: Option[(Int,Int,Boolean)], b: Option[(Int,Int,Boolean)]) = (a,b) match {
  			case (Some((a1,a2,x)), Some((b1,b2,y))) => Some((a1, b2, x && y && a2 <= b1))
  			case (a, None) => a
  			case (None, b) => b
  		}
  		val zero = None
  	}
  	foldMapV(ints, m)(i => Some((i,i,true))).map(_._3).getOrElse(true)
  }
  
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

	// Ex 10.10 Write a monoid instance for WC and make sure that it meets the monoid laws.

	// Ex 10.11 Use the WC monoid to implement a function that counts words in a String by 
  // recursively splitting it into substrings and counting the words in those substrings.

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
  	def op(a: WC, b: WC) = (a,b) match {
  		case (Part(al, aw, ar), Part(bl, bw, br)) => {
  			if((ar+bl).isEmpty) Part(al, aw+bw, br)
  			else Part(al, aw+bw+1, br)
  		}
  		case (Stub(a), Stub(b)) => Stub(a+b)
  		case (Stub(a), Part(l, w, r)) => Part(a+l, w, r)
  		case (Part(l, w, r), Stub(b)) => Part(l, w, r+b)
  	}
  	// initially I used Stub("") for the zero...
  	val zero = Stub("")
		// ...but the hint says a Stub should never be empty, so I switched zero to
  	// val zero = Part("", 0, "") ...then noticed the official answer also uses Stub("").
  }

  def count(s: String): Int = sys.error("todo")

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

