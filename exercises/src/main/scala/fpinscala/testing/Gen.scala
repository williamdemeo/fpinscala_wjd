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
	def &&(p: Prop): Prop = this && p
}

object Prop {
	type FailedCase = String // a message (possibly) indicating why a test failed
	type SuccessCount = Int  // number of successful tests run
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def choose(start: Int, stopExclusive: Int): Gen[Int] = ???
}

// Gen[A] is something that knows how to generate inhabitants of A.
trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

