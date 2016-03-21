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

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


trait Prop {
	// In case property test fails, `check` returns a Left((st,n)), where
	// st is a string that represents the value that caused the failure, and 
	// n  is the number of successful cases checked before failure occurred.
	def check: Either[(FailedCase, SuccessCount), SuccessCount]
	//def &&(that: Prop): Prop = new Prop { def check = Prop.this.check && that.check}
}

object Prop {
	type FailedCase = String // a message (possibly) indicating why a test failed
	type SuccessCount = Int  // number of successful tests run
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

  
case class Gen[A](sample: State[RNG, A]){
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

  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???

}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit[RNG,A](a)) // (Scala will infer the type of State.unit here.)
  
  // Gen.listOf could be a function with the signature Gen[Int] => Gen[List[Int]]. But since it doesn't seem 
  // like Gen.listOf should care about the type of the Gen it receives as input let's make it polymorphic: 
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  /* Notice what we're not specifying--the size of the list to generate. For this to be implementable, our 
   * generator must therefore either assume or be told the size. Assuming a size seems a bit inflexible--any 
   * assumption is unlikely to be appropriate in all contexts. So it seems that generators must be told the 
   * size of test cases to generate. We can imagine an API where this is made explicit:
   */
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]= Gen(State.sequence(List.fill(n)(a.sample))) 
  // Here, List.fill(n)(a.sample) results in a List[Gen[A]].

  /* This would certainly be a useful combinator, but not having to explicitly specify sizes is powerful 
   * as well. It means that whatever function runs the tests has the freedom to choose test case sizes, 
   * which opens up the possibility of doing the test case minimization we mentioned earlier. If the sizes 
   * are always fixed and specified by the programmer, the test runner won't have this flexibility.
   */

  // Ex 8.4 Implement Gen.choose using this representation of Gen . 
  // It should generate integers in the range start to stopExclusive.
  def choose(start: Int, stopExclusive: Int): Gen[Int] = 
    Gen ( State(RNG.nonNegativeInt).map { n => start + n % (stopExclusive -start) } )
    // Recall, RNG.nonNegativeInt : RNG => (Int, RNG)

  /* It's hard not to get the impression that all these type aliases are obfuscating matters. 
   * Wouldn't it be easier if we simply let Gen wrap a function of type RNG => (A, RNG), 
   * rather than have it wrap a State that wraps a function of type RNG => (A, RNG)?  
   * The problem with this seemingly simpler solution is that it doesn't allow us to use the 
   * State class methods (like the map method we used above).
   */
    
}

trait SGen[+A] {

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





