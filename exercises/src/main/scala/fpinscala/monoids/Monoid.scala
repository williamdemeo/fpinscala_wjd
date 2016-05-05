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

import scala.language.higherKinds
import fpinscala.testing._
import fpinscala.state._
import Gen._
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
  	} // This implementation makes clear exactly what op does; but see more concise version below.
  	val zero = None 
  }

  // Use the orElse combinator of the Option trait:
  def optionMonoid[A] = new Monoid[Option[A]] {
  	def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
  	val zero = None 
  }

  // Ex 10.3 A function having the same argument and return type is sometimes called an endofunction. 
  // Write a monoid for endofunctions.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  	def op(f: A => A, g: A => A) = f compose g // x -> f(g(x)) 
  	val zero = (a:A) => a
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

 	// Not sure what trimMonoid is for. I don't think it's mentioned in the book.
  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  /** Sec 10.2 Folding lists with monoids
   *  Monoids have an intimate connection with lists. If you look at the signatures of 
   *  foldLeft and foldRight on List, you might notice something about the argument types:
   *      def foldRight[B](z: B)(f: (A, B) => B): B
   *      def foldLeft[B](z: B)(f: (B, A) => B): B
   *  What happens when A and B are the same type?
   *      def foldRight(z: A)(f: (A, A) => A): A
   *      def foldLeft(z: A)(f: (A, A) => A): A
   *  The components of a monoid fit these argument types like a glove. So if we had a 
   *  list of Strings, we could simply pass the op and zero of the `stringMonoid` in order 
   *  to reduce the list with the monoid and concatenate all the strings:
   *      val words = List("Hic", "Est", "Index")
   *      val s = words.foldRight(stringMonoid.zero)(stringMonoid.op)
   *  Results in "HicEstIndex", and we get the same result with foldLeft, by associativity.
   *  `foldLeft` associates op to the left, whereas `foldRight` associates to the right. 
   */
  /* We can write a general function concatenate that folds a list with a monoid.
   * More precisely, if we have a list whose elements inhabit a type A that can serve as
   * the carrier of a monoid, then we can use the monoid's zero and binary op to perform a 
   * fold:
   */
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
  /* This shouldn't be called "concatenate."  Only for lists of strings or lists of lists
   * will this act like a concatenation operation.  Otherwise, values will be folded up,
   * and squashed. Do we call the sum of a list of ints "concatenation?"  I think "reduce" 
   * is a much better name for this function. Just for fun, let's define reduce that does 
   * exactly the same thing but, but use foldRight this time. (No difference by associativity.) 
   */
  def reduce[A](as: List[A], m: Monoid[A]): A = as.foldRight(m.zero)(m.op)
    
  /* But what if our list has an element type that doesn't have a Monoid instance? 
   * Well, we can always map over the list to turn it into a type that does:
   */
  // Ex 10.5 Implement foldMap.
	def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = (as.map(f)).foldRight(m.zero)(m.op)
  // (Left off here.  Next: check foldMap, foldRight, and foldLeft against the official solution.)
	
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

	// Ex 10.6 (Hard) foldMap can be implemented using either foldLeft or foldRight. 
  // But you can also write foldLeft and foldRight using foldMap! Try it.  
  def foldRight_via_foldMap[A,B](as: List[A])(z: B)(f: (A,B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)
	  // Originally write this as follows:
    //     foldMap(as, endoMonoid[B])(a => (b => f(a,b)))(z) 
    // But a => b => f(a,b) is f.curried.

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
  
  /**
   * WC is a trait that could be useful in a program that counts words in a document.
   */
  sealed trait WC
  /**
   * Stub is a string that models word segments.
   * When we split up a file into parts, the ends of each part may contain partial words. 
   * These partial words, or word segments, are what we call Stubs.
   */
  case class Stub(chars: String) extends WC
  /**
   * A Part object counts the words in a single part of a file. 
   * We split up a file into parts, and each part is a `Part` instance, which has a `wc` field
   * (the number of whole words in the part), an `lStub` (the partial 
   * word at the beginning of the part), and an `rStub` (the partial word at the end of the part).
   */
  case class Part(lStub: String, wc: Int, rStub: String) extends WC

	// Ex 10.10 Write a monoid instance for WC and make sure that it meets the monoid laws.

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

  /** 
   * A generator for Part cases of WC.  This is sloppy, but it works. 
   * There's a better way of composing RNG's without passing them along manually.
   * @todo: fix this.    
   */
  def genPart(sg: Gen[String]): Gen[WC] = Gen {
    State { (rng: RNG) =>
        val (s1, rng1) = sg.sample.run(rng)
        val (wc, rng2) = rng1.nextInt
        val (s2, rng3) = sg.sample.run(rng2)
        (new Part(s1, wc, s2), rng3)
        } 
  }

   // A generator for Stub cases of WC.
  def genStub(sg: Gen[String]): Gen[WC] = Gen {
  	State{ (rng: RNG) => 
  		val (s1, rng1) = sg.sample.run(rng)
  		(new Stub(s1), rng1)
  	}
  }
  
  // So we don't have to think about it, the next method supplies the string generator to 
  // genPart for us, using an arbitrarily chosen max string length of 10.
  val autoPart: Gen[WC] = genPart( Gen.stringGenN( choose(0, 10) ) )
  val autoStub: Gen[WC] = genStub( Gen.stringGenN( choose(0, 10) ) )
  // Finally, our WC generator, which gives equal weight to Parts and Stubs.
	// This was created for the sole purpose of testing that wcMonoid satisfies the monoid laws.  
  val genWC: Gen[WC] = union(autoPart, autoStub)

  // Finally, our WC generator, which gives equal weight to Parts and Stubs.
	// This was created for the sole purpose of testing that wcMonoid satisfies the monoid laws.  
  val genWC_weighted: Gen[WC] = weighted((autoPart,.75), (autoStub,.25))

  
  // Ex 10.11 Use the WC monoid to implement a function that counts words in a String by 
  // recursively splitting it into substrings and counting the words in those substrings.
  // (skipping this for now)

  def count(s: String): Int = sys.error("todo")

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")

} // <<<<<<<<<<<<<<<<<<<< END: Monoid companion object <<<<<<<<<<<<<<<<<<<<

/* Section 10.5 (p. 183)
 * Here we're abstracting over a type constructor F , much like we did with the Parser 
 * type in the previous chapter. We write it as F[_] , where the underscore indicates that F
 * is not a type but a type constructor that takes one type argument. Just as functions of
 * functions are called higher-order functions, `Foldable` and `F[_]` are both higher-order 
 * type constructors, aka "higher-kinded types."
 */
trait Foldable[F[_]] {
  import Monoid._  // (not sure why we need this)
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](as: F[A]): List[A] = sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  // We simply defer to the original defs in the List companion object.
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  // what's the difference between `as.foldRight(z)(f)` and `foldRight(as)(z)(f)` here?
  override def foldRight_v2[A, B](as: List[A])(z: B)(f: (A, B) => B) = foldRight(as)(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B)= foldLeft(as)(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = foldMap(as)(f)(mb)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  // We simply defer to the original defs in the IndexedSeq companion object.
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = foldRight(as)(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = foldLeft(as)(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = foldMap(as)(f)(mb)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = foldRight(as)(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = foldLeft(as)(z)(f)
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

