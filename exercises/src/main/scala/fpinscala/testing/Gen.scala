/* File: Gen.scala (Ch 8)
 * Authors: Paul Chiusano and Runar Bjarnason
 * Url: https://github.com/fpinscala/fpinscala 
 * 
 * Description: This is a modified version of the file Gen.scala
 *   that accompanies the book "Functional Programming in Scala" by
 *   Chiusano and Bjarnason. This version of the file includes 
 *   solutions to some of the exercises in 
 * 
 *     CHAPTER 8: Property-based testing
 * 
 *   The solutions herein are by William DeMeo <williamdemeo@gmail.com>.
 *   They are at best imperfect, and possibly wrong.  Official solutions by 
 *   Chiusano and Bjarnason are available in the github repo mentioned above.
 */
package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/* _Justification for the new type called `Return`_ (cf. p.133 of fpinscala)
 * Initially, the return type of a Prop was Option, with None returned in case of failure.
 * This seemed reasonable, but it was refined so we could get a message indicating reason for 
 * failure and an int showing how many tests passed before the failure.  The Either type seem
 * appropriate for this purpose.  Refining further, we decided that the number of tests to run should
 * be passed in to the Prop type itself.  Therefore, if all tests pass, there's no reason to report
 * the number of successes, since it will equal the input parameter in all such cases.  So, we are
 * back to the Option type.  But it's strange to use the Option type when we want None to denote 
 * success and Some((message, n)) to denote failure.  We want the return type of Prop to clearly 
 * represent our intention and the Option type seems to be the opposite of our intention.  So we give 
 * up on the built-in types and create our own that is the most appropriate return type for Prop. 
 * We name this type `Result.`
 */
// Result type is the return type of a Prop's run method.
sealed trait Result { def isFalsified: Boolean }

// A Prop's run method returns Proved if there is only one thing to test and it passed.
case object Proved extends Result { def isFalsified = false }

// A Prop's run method will return the Passed object if the test passes.
case object Passed extends Result { def isFalsified = false }

// A Prop's run method will return a Falsified object in case of failure.
case class Falsified (failure_string: FailedCase, 
                      num_successful: SuccessCount) 
                      extends Result { def isFalsified = true }


case class Prop( run: (Int, RNG) => Result ) {
  // Ex 8.9a Implement && for composing Prop values.
  def &&(that: Prop): Prop = Prop ( (n, rng) => { 
    val res1 = this.run(n,rng)
    res1 match {
        case Passed => that.run(n,rng)
        case _ => res1
      } }
  )
  // Ex 8.9b Implement || for composing Prop values.
  def ||(that: Prop): Prop = Prop ( (n, rng) => {
    val res1 = this.run(n,rng)
    res1 match {
        case Passed => res1
        case _ => that.run(n,rng)
      }   }
  )
  // Notice that in the case of failure we don't know which property was responsible, the left or 
  // the right. Can you devise a way of handling this? perhaps by allowing Prop values to be 
  // assigned a tag or label which gets displayed in the event of a failure?
}

object Prop {
	type FailedCase = String // (type alias) this is the type of strings that describe a test failure
	// This could be a message (possibly) indicating why a test failed.
	type SuccessCount = Int  // (type alias) for int values indicating how many tests have passed

	def forAll[A](as: Gen[A])(predicate: A => Boolean): Prop = Prop {
	  // Use randomStream to get random stream of A's, then zip that with a stream of ints to get a stream of 
	  // pairs of type (A, Int). Then, take(n) from the stream of pairs apply the map which tests the predicate.
	  (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
	    case (a, i) => try {
	      if (predicate(a)) Passed else Falsified(a.toString, i)
	    } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
	  }.find(_.isFalsified).getOrElse(Passed)
	}

	def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
	
	def buildMsg[A](s: A, e: Exception): String =
	  s"test case: $s\n" +
	  s"generated an exception: ${e.getMessage}\n" +
	  s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

  
case class Gen[+A](sample: State[RNG, A]){
/* Gen[A] is something that knows how to generate values of type A. It could randomly generate these values. 
 * We already developed an interface for a purely functional random number generator RNG (Ch. 6), and we 
 * showed how to make it convenient to combine computations that made use of it. 
 * 
 * We let `Gen` be a type that wraps a state transition over a random number generator: 
 * 
 *   case class Gen[A](sample: State[RNG,A]) 
 * 
 * Recall `case class State[S,A](run: S => (A,S))` so the `sample` function wrapped inside a Gen should be
 * a function that gets us from state s1:RNG to a pair (a, s2):(A, RNG).
 */
  
  def map[A,B](f: A => B): Gen[B] = ???

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(this.sample.flatMap[B](a => f(a).sample)) 

  def listOfSize(size: Int): Gen[List[A]] = Gen.listOfSizeN(size, this)

  def listOfN(gsize: Gen[Int]): Gen[List[A]] = gsize flatMap(n => this.listOfSize(n))
  
  // Ex 8.10 Implement helper functions for converting Gen to SGen.
  def unsized: SGen[A] = SGen(_ => this)    
}

object Gen {
  // Ex 8.4 Implement Gen.choose which generates ints in the range [start, stopExclusive).
  def choose(start: Int, stopExclusive: Int): Gen[Int] = 
    Gen ( State(RNG.nonNegativeLessThan(stopExclusive -start)).map(_ + start))
    // RNG.nonNegativeLessThan returns a Rand[Int], which is an alias for RNG => (Int, RNG).
    // nonNegativeLessThan(b-a) gives a random int in [0, b-a).
    // map(_+a) take that int to the interval [a, b), as desired.
    // This is simpler than the official solution.

  /* It's hard not to get the impression that all these type aliases are obfuscating matters. 
   * Wouldn't it be easier if we simply let Gen wrap a function of type RNG => (A, RNG), 
   * rather than have it wrap a State that wraps a function of type RNG => (A, RNG)?  
   * The problem with this seemingly simpler solution is that it doesn't allow us to use the 
   * State class methods (like the map method we used above).
   */
  // Ex 8.5 Let's see what else we can implement using this representation of Gen . 
  // Try implementing unit, boolean, and listOfN.
  def unit[A](a: => A): Gen[A] = Gen(State.unit[RNG,A](a)) // (Scala will infer the type of State.unit here.)

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def double: Gen[Double] = Gen(State(RNG.double))
  
  def listOfSizeN[A](n: Int, g: Gen[A]): Gen[List[A]]= Gen(State.sequence(List.fill(n)(g.sample))) 
  // Here, List.fill(n)(g.sample) results in (g.sample, g.sample, ..., g.sample): List[State[S,A]].
	/* This would certainly be a useful combinator, but not having to explicitly specify sizes is powerful 
   * as well. It means that whatever function runs the tests has the freedom to choose test case sizes, 
   * which opens up the possibility of doing the test case minimization we mentioned earlier. If the sizes 
   * are always fixed and specified by the programmer, the test runner won't have this flexibility.
   */

    
  // p.131: If we can generate a single Int in some range, do we need a new primitive to 
  // generate an (Int,Int) pair in some range? Answer: No, we don't need new primitives.
  // It's very easy to get a list of two random integers in the interval [a,b):
  def intListOfTwoInts(a: Int, b: Int): Gen[List[Int]] = listOfSizeN[Int](2, choose(a,b))  
  // It's only slightly harder to get a pair of two random integers in the interval [a,b):
  def intPair(a: Int, b: Int): Gen[(Int,Int)] = 
    Gen(listOfSizeN[Int](2, choose(a,b)).sample.map{ case List(x,y) => (x,y) })  

  // p.131: Can we produce a Gen[Option[A]] from a Gen[A]? Answer: yes.
  def genToOpt[A](g: Gen[A]): Gen[Option[A]] = Gen(g.sample.map[Option[A]](a => Some(a)))
  // p.131: What about a Gen[A] from a Gen[Option[A]]?  
  // Answer: Yes, if we know what to do with None cases.  Here's one possibility:
  def genFromOpt[A](g: Gen[Option[A]]): Gen[A] = 
    Gen(g.sample.map[A]{
        case None => sys.error("None")
        case Some(a) => a
        }
    ) 

  // Ex 8.7 Implement union, for combining two generators of the same type into one, by pulling
  // values from each generator with equal likelihood.
  def union_first_try[A](g1: Gen[A], g2: Gen[A]): Gen[A] = 
    Gen ( State ( rng => RNG.boolean(rng) match {
      case (true,  _) => g1.sample.run(rng)
      case (false, _) => g2.sample.run(rng)
    } ) )
  // Actually, it's much easier than this.  We simply want to return g1 (or g2) itself, depending 
  // on the value of a randomly generated boolean, so...
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap[A](x => if(x) g1 else g2)
      
  // Ex 8.8 Implement weighted, a version of union that accepts a weight for each Gen and 
  // generates values from each Gen with probability proportional to its weight.
  def weighted[A](t1: (Gen[A],Double), t2: (Gen[A],Double)): Gen[A] = 
    double.flatMap[A]( x => if( x < t1._2 ) t1._1 else t2._1 )   
    
}

case class SGen[+A](forSize: Int => Gen[A]){

  // Ex 8.11 SGen supports many of the same ops as Gen. Define some convenience functions on SGen that 
  // simply delegate to the corresponding functions on Gen.
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => this.forSize(n).flatMap { x => f(x).forSize(n) })
  // Not sure if flatMap is correct.  Better check it.

  // Ex.12 Implement a listOf combinator that doesn't accept an explicit size. It should return an
  // SGen instead of a Gen. The implementation should generate lists of the requested size.
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfSize(n))

  
}



/* Ex 8.1 To get used to thinking about testing in this way, come up with properties that
 * specify the implementation of a sum: List[Int] => Int function. You don't have to write 
 * your properties down as executable ScalaCheck code--an informal description is fine.
 * Here are some ideas to get you started:
 *     Reversing a list and summing it should give the same as summing the original list.
 *     What should the sum be if all elements of the list are the same value?
 *     Can you think of other properties?
 *     
 * Possible Answers: 
 *     - check commutative and associative laws?
 *     - summing a list of 0's should equal 0.
 *     - summing an n-element list where all elements are c should equal n*c.
 *     - sum(h::tail) should equal h + sum(tail)
 *     - sum(Nil) = sys.error?
 *     - sum(l1 append l2) = sum(l1) + sum(l2)
 *     - sum(l1 append (l2 append l3)) = sum(l1) + sum(l2) + sum(l3) = sum((l1 append l2) append l3)
 *       (This test wouldn't be necessary if we have already tested append such that the
 *       two arguments to sum that appear here give the same list.) 
 */

/* Ex 8.2 What properties specify a function that finds the maximum of a List[Int]?
 * 
 * Answer:
 *     l.forAll(x => (x <= maximum)) // every element of the list is bounded above by maximum
 *     && l.exists(x => (x == maximum)) // the maximum occurs in the list.
 * 
 */

/* Ex 8.3 Assuming the following representation of Prop, implement && as a method of Prop.
 *         
 *    trait Prop { def check: Boolean }
 *     
 * Answer: 
 * 
 * 		trait Prop { 
 * 			def check: Boolean
 * 			def &&(that: Prop): Prop = new Prop { 
 * 				def check = Prop.this.check && that.check
 * 			}
 */





