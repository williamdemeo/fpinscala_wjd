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
  def optionMonoid[A] = new Monoid[Option[A]] {
  	def op(a1: Option[A], a2: Option[A]) = (a1,a2) match {
  		case (Some(a), _) => Some(a)
  		case (None, Some(b)) => Some(b)
  		case (None, None) => None
  	}
  	val zero = None 
  }

  // Ex 10.3 A function having the same argument and return type is sometimes called an endofunction. 
  // Write a monoid for endofunctions.
  def endoMonoid[A] = new Monoid[A => A] {
  	def op(f: A => A, g: A => A) = {a => f(g(a))}
  	def zero = {a => a}
  }

	// Ex 10.4 Use the property-based testing framework we developed in part 2 to implement a
	// property for the monoid laws. Use your property to test the monoids we’ve written.	
  def monoidLawsPredicate[A](m: Monoid[A])(t: (A,A,A)): Boolean =	t match { 
  	case (a,b,c) => {
  		(m.op(m.op(a, b),c) == m.op(a, m.op(b,c))) && // associative
  		(m.op(a, m.zero)== a) && (m.op(m.zero,a)==a)  // identity
		}
  }
  def monoidLaws_first_try[A](m: Monoid[A], gen: Gen[A]): Prop =
  	forAll(Gen.triple(gen))(monoidLawsPredicate(m))

  // Second try (not sure it's clearer, but it is more compact)
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  	// associative law
  	forAll(Gen.triple(gen))(t => 
  		(m.op(m.op(t._1, t._2),t._3) == m.op(t._1, m.op(t._2, t._3)))) &&
 		// identity law
  	forAll(gen)(a => (m.op(a, m.zero)== a) && (m.op(m.zero,a)==a))  

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    sys.error("todo")

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    sys.error("todo")

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    sys.error("todo")

  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    sys.error("todo")

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    sys.error("todo") 

  val wcMonoid: Monoid[WC] = sys.error("todo")

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

