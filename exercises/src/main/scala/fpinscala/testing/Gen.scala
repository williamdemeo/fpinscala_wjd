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
	//     st is a string that represents the value that caused the failure, and 
	//     n  is the number of successful cases checked before failure occurred.
	def check: Either[(FailedCase, SuccessCount), SuccessCount]
	def &&(that: Prop): Prop = this && that
}

object Prop {
	type FailedCase = String // a message (possibly) indicating why a test failed
	type SuccessCount = Int  // number of successful tests run
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  
  /* We determined earlier that a Gen[A] was something that knows how to generate values of type A . 
   * What are some ways it could do that? Well, it could randomly generate these values. 
   * Look back at the example from chapter 6--there, we gave an interface for a purely functional 
   * random number generator RNG and showed how to make it convenient to combine computations that 
   * made use of it. We could just make Gen a type that wraps a State transition over a random number 
   * generator: 
   * 
   *   case class Gen[A](sample: State[RNG,A]) 
   * 
   * Recall, the definition: case class State[S,A](run: S => (A,S)) 
   */
  // Ex 8.4 Implement Gen.choose using this representation of Gen . 
  // It should generate integers in the range start to stopExclusive.
  def choose(start: Int, stopExclusive: Int): Gen[Int] = ???
}

// Gen[A] is something that knows how to generate inhabitants of A.
trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
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
 * 			def &&(that: Prop): Boolean = this && that
 * 		}
 */






