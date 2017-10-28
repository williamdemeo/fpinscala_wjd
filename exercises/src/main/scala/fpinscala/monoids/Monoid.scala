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

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import fpinscala.state._
//import fpinscala.testing.Gen._
//import fpinscala.testing.Prop._

import scala.language.higherKinds // infix syntax for `Par.map`, `Par.flatMap`, etc

trait Monoid[A] {
  def op(a1: A, a2: A): A
  val zero: A
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

  //== EXERCISE 10.1 ============================================================
  // Give Monoid instances for integer add and multiply as well as Boolean operators.
  val intAdditive = new Monoid[Int] {
  	def op(a1: Int, a2: Int) = a1+a2
  	val zero = 0
  }
  // Adding implicit declaration so that we don't have to mention the monoid and we can do
  // `reduce(List(1,2,3))`, for example, instead of `reduce(List(1,2,3), m:Monoid[Int])`
  implicit val IntAdditive = intAdditive

  val intMultiplicative = new Monoid[Int] {
  	def op(a1: Int, a2: Int) = a1*a2
  	val zero = 1
  }
  // N.B. the following would make the implicit monoid for Int type ambiguous.
  //     implicit val IntMultiplicative = intMultiplicative
  // Define only use one implicit per ground type!

  val booleanOr = new Monoid[Boolean]{
  	def op(a1: Boolean, a2: Boolean) = a1 || a2
  	val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
  	def op(a1: Boolean, a2: Boolean) = a1 && a2
  	val zero = true
  }
  implicit val BooleanAnd = booleanAnd
  // booleanAnd will be the more commonly used Monoid structure over Boolean, because we often
  // take the conjunction of a list of predicates, so the default fold should be conjunction.
  //== END EXERCISE 10.1 ===========================================================


  //== EXERCISE 10.2 ===============================================================
  // Give a Monoid instance for combining Option values.
  def optionMonoid_first_try[A] = new Monoid[Option[A]] {
  	def op(a1: Option[A], a2: Option[A]) = (a1,a2) match {
  		case (Some(a), _) => Some(a)
  		case (None, Some(b)) => Some(b)
  		case (None, None) => None
  	} // This version makes clear what op does; more concise version below.
  	val zero = None 
  }
  // Concise version using the orElse combinator of the Option trait:
  def optionMonoid[A] = new Monoid[Option[A]] {
  	def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
  	val zero = None 
  }
  //== END EXERCISE 10.2 ======================================


  //== EXERCISE 10.3 ===============================================================
  // A function having the same argument and return type is sometimes called an endofunction.
  // Write a monoid for endofunctions.
  // binary op: function composition; neutral element: the identity function.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  	def op(f: A => A, g: A => A) = f compose g // x -> f(g(x)) 
  	val zero = (a:A) => a  // zero is the identity function
  }
  implicit def endoMonoid_i[A] = endoMonoid[A]
  //== END EXERCISE 10.3 ======================================

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  //== EXERCISE 10.4 ===============================================================
  // Use the property-based testing framework we developed in part 2 to implement a
	// property for the monoid laws. Use your property to test the monoids we've written.
  // (monoidLaws testing fn defined below, but actual tests are in Ch10_monoids.sc)
  import fpinscala.testing._
  import Prop._
  import Gen._
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
  //== END EXERCISE 10.4 ======================================

 	// Not sure what trimMonoid is for. I don't think it's mentioned in the book.
  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  /** SECTION 10.2: FOLDING LISTS WITH MONOIDS *************************************
    *
    *  What happens in fold operations when A and B are the same type?
    *      def foldRight(z: A)(f: (A, A) => A): A
    *      def foldLeft(z: A)(f: (A, A) => A): A
    *  The components of a monoid fit these argument types like a glove.
    *
    *  Indeed, we can write a general function `concatenate` (I prefer "reduce")
    *  that folds a list with a monoid. More precisely, given a list with elements
    *  inhabiting a type A that can be given a monoid structure, we can use the
    *  (monoid) zero and binary op to perform a fold:
    */

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
  /* This shouldn't be called "concatenate."  Only for lists of strings or lists of lists
   * will this act like concatenation.  Otherwise, values are folded up and squashed.
   * I think "reduce" is a much better name for this. Also, we can clean this up using
   * implicits.  Let's define `reduce` to behave just like concatenate but, for fun,
   * we'll define it with foldRight, and use implicits so no need to mention the monoid
   * explicitly when using `reduce`.
   */ 
  def reduce[A](as: List[A])(implicit m: Monoid[A]): A = as.foldRight(m.zero)(m.op)
  // For more details about why/how this works, see for example:
  //  https://tpolecat.github.io/2013/10/12/typeclass.html
  // Now we should be able to simply do `reduce(List(1,2,3))`
    
  /* But what if we have a List[A] and the type A doesn't have a Monoid instance? 
   * If we happen to have a function f: A => B where B is a Monoid instance, then
   * we can first map the List[A] to a List[B], and then perform the fold.
   */

  //== EXERCISE 10.5 =====================================================
  // Implement foldMap.
	def foldMap_first_try[A, B](as: List[A], m: Monoid[B])(f: A => B): B = 
	  (as.map(f)).foldRight(m.zero)(m.op)
  // In fact, we don't even need to use map; we can get by with just foldRight.
	def foldMap_second_try[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
	  as.foldRight(m.zero)((a,b) => m.op(f(a),b))
  // This might be even better:
	def foldMap_imp[A, B](as: List[A])(f: A => B)(implicit m: Monoid[B]): B = 
	  as.foldRight(m.zero)((a,b) => m.op(f(a),b))
  // So, if there's an implicit monoid instance defined for type B, then we can 
  // simply do `foldMap_imp(as)(f) instead of foldMap(as,m)(f)`
  // Next: check foldMap, foldRight, and foldLeft against the official solution.
  // Here's the OFFICIAL SOLUTION:
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  //== END EXERCISE 10.5 ======================================

	  
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

	//== EXERCISE 10.6 (Hard) ==========================================================
  // foldMap can be implemented using either foldLeft or foldRight.
  // But you can also write foldLeft and foldRight using foldMap! Try it.  
  def foldRight_via_foldMap[A,B](as: List[A])(z: B)(f: (A,B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)
  // Same as above but using implicits (so we don't have to mention endoMonoid)
  def foldRight_imp[A,B](as: List[A])(z: B)(f: (A,B) => B): B = {
    val fmap =foldMap_imp(as)(f.curried)  // We can't simply use an anonymous function...  
    fmap(z)}                             // ...so use of implicits is not much of a win here.
  def swap_curried[A,B,C](f: (B, A) => C) = (a: A) => (b:B) => f(b,a)
  def foldLeft_via_foldMap[A,B](as: List[A])(z: B)(f: (B,A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(swap_curried(f))(z)
  def foldLeft_via_foldMap_alt[A,B](as: List[A])(z: B)(f: (B,A) => B): B =
    foldMap(as, dual(endoMonoid[B]))((a:A) => (b:B) => f(b,a))(z)
  //== END EXERCISE 10.6 ======================================


	//== EXERCISE 10.7 ===================================================================
  // Implement a foldMap for IndexedSeq. Your implementation should use the strategy
	// of splitting the sequence in two, recursively processing each half, and then adding the
	// answers together with the monoid.
	def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = 
		if (v.size==0) m.zero
		else if (v.size==1) f(v(0))
		else {
			val (l,r)=v.splitAt(v.length/2)
			m.op( foldMapV(l, m)(f), foldMapV(r, m)(f) )
		}
  //== END EXERCISE 10.7 ======================================

	//== Ex 10.8 (Hard) ========================================================
  // Implement a parallel version of foldMap using the library from ch 7.
	// Hint: Implement par, a combinator to promote Monoid[A] to a Monoid[Par[A]], and
  // then use this to implement parFoldMap.
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
  	def op(a1: Par[A], a2: Par[A]) = a1.map2(a2)(m.op)
    // old code: `Par.map2(a1,a2)(m.op)` but import Par.toParOps allows infix notation
  	val zero = Par.unit(m.zero)
  }
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
	  Par.flatMap(Par.parMap(v)(f)) { bs => foldMapV( bs, par(m) )( b => Par.lazyUnit(b) ) }
  //== END EXERCISE 10.8 ======================================

  //== Ex 10.9 (Hard) ====================================================
  // Use foldMap to detect whether a given IndexedSeq[Int] is ordered.
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
  //== END EXERCISE 10.9 ======================================

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

	//== EXERCISE 10.10 ====================================================================
  // Write a monoid instance for WC and make sure that it meets the monoid laws.
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
   * todo: fix this.
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
  val autoPart: Gen[WC] = genPart( stringGenN( choose(0, 10) ) )
  val autoStub: Gen[WC] = genStub( stringGenN( choose(0, 10) ) )
  // Finally, our WC generator, which gives equal weight to Parts and Stubs.
	// This was created for the sole purpose of testing that wcMonoid satisfies the monoid laws.  
  val genWC: Gen[WC] = union(autoPart, autoStub)

  // Finally, our WC generator, which gives equal weight to Parts and Stubs.
	// This was created for the sole purpose of testing that wcMonoid satisfies the monoid laws.  
  val genWC_weighted: Gen[WC] = weighted((autoPart,.75), (autoStub,.25))
  //== END EXERCISE 10.10 ========================================================

  //== EXERCISE 10.11 ============================================================
  // Use the WC monoid to implement a function that counts words in a String by
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
  //== END EXERCISE 10.11 ============================================================

} // <<<<<<<<<<<<<<<<<<<< END: Monoid companion object <<<<<<<<<<<<<<<<<<<<

/** SECTION 10.5 (p. 183) **********************************************************
 * Here we're abstracting over a type constructor F. We write it as F[_] , where the
 * underscore means F is not a type but a type constructor that takes one type argument.
 * Just as functions of functions are called higher-order functions, `Foldable` and
 * `F[_]` are both higher-order type constructors, aka "higher-kinded types."
 */
trait Foldable[F[_]] {
  import Monoid.{dual, endoMonoid}
  // default implementations
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = 
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = 
    foldMap(as)((a:A) => (b:B) => f(b,a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(implicit mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a:A, b:B) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def reduce[A](as: F[A])(implicit m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] = foldRight(as)(List[A]())(_::_)
}

//== EXERCISE 10.12 ================================================
// Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].
// Remember that foldRight, foldLeft, and foldMap can all be implemented in terms
// of each other, but that might not be the most efficient implementation.
object ListFoldable extends Foldable[List] {
  // We simply defer to the original defs in the List companion object.
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = foldLeft(as)(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(implicit mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a,b) => mb.op(f(a),b))
  override def toList[A](as: List[A]): List[A] = as
}
object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid.foldMapV
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(implicit mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}
object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
}
//== END EXERCISE 10.12 ================================================

//== EXERCISE 10.13 ================================================================
// Recall the binary Tree data type from chapter 3. Implement a Foldable instance for it.
// sealed trait Tree[+A]
// case object Leaf[A](value: A) extends Tree[A]
// case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(implicit mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}
//== END EXERCISE 10.13 =====================================================

//== EXERCISE 10.14 ================================================================
// Write a Foldable[Option] instance.
object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(implicit mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}
//== END EXERCISE 10.14 =====================================================


/////////////////////////////////////////////////////////////////////////////////
//
//  -- MISC NOTES --
// (removed from above and placed here to clean things up a bit)
//
/** Sec 10.2 Folding lists with monoids *******************************************
 *  Monoids have an intimate connection with lists. If you look at the signatures of
 *  foldLeft and foldRight on List, you notice something about the argument types:
 *      def foldRight[B](z: B)(f: (A, B) => B): B
 *      def foldLeft[B](z: B)(f: (B, A) => B): B
 *  What happens when A and B are the same type?
 *      def foldRight(z: A)(f: (A, A) => A): A
 *      def foldLeft(z: A)(f: (A, A) => A): A
 *  The components of a monoid fit these argument types like a glove. So if we had a
 *  list of Strings, we could pass the op and zero of the `stringMonoid` in order
 *  to reduce the list with the monoid and concatenate all the strings:
 *      val words = List("Hic", "Est", "Index")
 *      val s = words.foldRight(stringMonoid.zero)(stringMonoid.op)
 *  Results in "HicEstIndex"; we get the same result with foldLeft, by associativity.
 *  (`foldLeft` associates op to the left; `foldRight` associates to the right)
 ***********************************************************************************/
