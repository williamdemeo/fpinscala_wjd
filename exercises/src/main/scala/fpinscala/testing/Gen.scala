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


//==== begin: Prop class ============================================================================
case class Prop( run: (MaxTestSize, NumberOfTests, RNG) => Result ) {

	// Ex 8.9a Implement && for composing Prop values.
  def &&(that: Prop): Prop = Prop { 
    (maxsize, numtests, rng) => this.run(maxsize, numtests, rng) match {
        case Passed | Proved => that.run(maxsize, numtests, rng)
        case notPassedOrProved => notPassedOrProved //
      }
  }
  // Ex 8.9b Implement || for composing Prop values.
  def ||(that: Prop): Prop = Prop {
    (maxsize, numtests, rng) => this.run(maxsize, numtests, rng) match {
      case Falsified(msg, _) => that.tag(msg).run(maxsize, numtests, rng)
      case notFalsified => notFalsified
    }
  }
  // The tag method provides a way of testing a Prop and, upon failure, prepend
  // the given message to the Prop's own fail message.
  def tag(msg: String) = Prop {
    (maxsize, numtests, rng) => this.run(maxsize, numtests, rng) match {
      case Falsified(mymsg, numsuc) => Falsified(msg+"\n" + mymsg, numsuc)
      case notFalsified => notFalsified
    }
  }
  // Notice that in the case of failure we don't know which property was responsible, the left or 
  // the right. Can you devise a way of handling this? perhaps by allowing Prop values to be 
  // assigned a tag or label which gets displayed in the event of a failure?
}
//===== end: Prop class =======================================================================


//===== BEGIN: Prop companion object ===========================================================
object Prop {
	type FailedCase = String // (type alias) strings that describe a test failure (indicate why a test failed)
	type SuccessCount = Int  // (type alias) how many tests have passed
	type NumberOfTests = Int
	type MaxTestSize = Int

	// Justification for new type called `Return`: (cf. p.133, fpinscala) 
	// (notes about this have been moved to bottom of this file)

	// Result type is the return type of a Prop's run method.
	sealed trait Result { def isFalsified: Boolean }

	// A Prop's run method returns Proved if there is only one thing to test and it passes.
	case object Proved extends Result { def isFalsified = false }

	// A Prop's run method will return the Passed object if all tests succeed.
	case object Passed extends Result { def isFalsified = false }

	// A Prop's run method will return a Falsified object in case of failure.
	case class Falsified (failure_string: FailedCase, num_successful: SuccessCount) 
		extends Result { 
			def isFalsified = true 
		}
	
	def apply(f: (NumberOfTests,RNG) => Result): Prop = Prop { (_,n,rng) => f(n,rng) }
	
	// run helper method
  def run(p: Prop,
  	      maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  
  //--- First implementation of forAll, for "static" test sizes ---
	/* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean) = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }
	
	def buildMsg[A](s: A, e: Exception): String =
	  s"test case: $s\n" +
	  s"generated an exception: ${e.getMessage}\n" +
	  s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  

	//--- We could also add a random predicate generator...
  def randomStreamWithPredicate[A,B](agen: Gen[A])(pred: Gen[A=>Boolean])(rng: RNG): 
  	Stream[(A, A=>Boolean)] = Stream.unfold(rng){rng => 
    	val (a, rng2) = agen.sample.run(rng)
    	val (p, rng3) = pred.sample.run(rng2)
    	Some((a,p),rng3)
    }
  // ...this would allow us to run forAll on generated A's *and* generated predicates.
  // It might seem weird because, usually, we have a specific prediate in mind that
  // we're trying to test on lots of randomly generated data. However... (this remark
  // is continued at bottom of this file)
  def forAll2[A](as: Gen[A])(fs: Gen[A => Boolean]) = Prop {
    (n,rng) => randomStreamWithPredicate(as)(fs)(rng).zip(Stream.from(0)).take(n).map {
      case ((a,f), i) => try {
        if (f(a)) Passed else Falsified((a,f).toString, i)
      } catch { case e: Exception => Falsified(buildMsg2(a,f,e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll3[A](as: Gen[A])(preds: Gen[A => Boolean]) = Prop { (n,rng) =>
  	// Start by generating a stream of properties to test:
		val pStream = randomStream(as)(rng) map (a => forAll(preds)(p => p(a)) ) // each case returns a Prop
		// Take the first n and, using && combinator, reduce them to a single conjunction: 
  	val pConj = pStream.take(n).toList.reduce(_ && _)
  	// Test the conjunction of n properties:
	  pConj.run(n, n, rng)  // todo: get rid of dependence on MaxTestSize
  }
	
	def buildMsg2[A](a: A, p: A=>Boolean, e: Exception): String =
	  s"test case val: $a\n" +
	  s"test case fun: $p\n" +
	  s"generated an exception: ${e.getMessage}\n" +
	  s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  

	//--- Second implementation of forAll, for "dynamic" test sizes ---
  def forAll[A](sg: SGen[A])(predicate: A => Boolean): Prop = forAll(sg.forSize)(predicate)

	def forAll[A](g: Int => Gen[A])(predicate: A => Boolean): Prop = Prop {
	  (maxSize, numTests, rng) => 
	  	val casesPerSize = (numTests + (maxSize - 1 )) / maxSize
	  	val props: Stream[Prop] = 
	  		Stream.from(0).take((numTests min maxSize) + 1).map( i =>	forAll(g(i))(predicate))	
  		val prop: Prop = props.map( p => 
  			Prop { (mx, _, rng) => p.run(mx, casesPerSize, rng) } ).toList.reduce(_ && _)
	  	prop.run(maxSize, numTests, rng)
	}
  
  //--- BEGIN: Helper methods for streamlining Par tests -------------------------------
  val ES: ExecutorService = Executors.newCachedThreadPool

  // check is for proving properties or laws.
  def check(p: => Boolean) = Prop { (_, _, _) =>	if (p) Proved else Falsified("()", 0)  }


  // Lift equality relation into Par using map2.
  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p1,p2)(_ == _)
  
  val S = weighted(
  		(choose(1,4).map(Executors.newFixedThreadPool), .75),  // create fixed thread pool (TP) 75% of the time
  		(unit(Executors.newCachedThreadPool), .25)  // create unbounded TP 25% of the time.
  	)
  	
 	def forAllPar[A](g: Gen[A]) (f: A => Par[Boolean]): Prop = 
 		forAll(S ** g) { case (s,a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)
  def provePar(p: Par[Boolean]) = Prop { (_,_,_) =>	
  	if (p(ES).get) Proved else Falsified("()", 0)  }
  //---- END: Helper methods for streamlining Par tests ----------------------------
  
//====== END: Prop companion object =============================================================
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
 * Recall `case class State[S,A](run: S => (A,S))` so the `sample` object wrapped inside a Gen 
 * has a run function that takes a rng1:RNG and gives back a pair (a, rng2): (A, RNG).
 */
  
  def map[B](f: A => B): Gen[B] = Gen(this.sample.map(f))
  // Recall, the map method of State[S,A] class is
  //   def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))
  
  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =	Gen(this.sample.map2(g.sample)(f))
	// Recall, the map2 method of State[S,A] class has signature
  //   def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C]

  // Ex 8.6 In the Gen class, implement `flatMap`, then use it to implement a more dynamic `listOfN`. 
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(this.sample.flatMap[B](a => f(a).sample)) 

  // f ** g is syntactic sugar for combining two generators f and g to produce a pair generator.
  def **[B] (g: Gen[B]): Gen[(A,B)] = this.map2(g)((_,_))

  // Generate a Gen[List[A]] with length of list given by input parameter `n` 
  def listOfLength(n: Int): Gen[List[A]] = Gen.listOfLength(n, this)
  // alias to listOfLength function of the companion object

  // Generate a Gen[List[A]] with length of list generated by the given int generator.
  def listOfGeneratedLength(glen: Gen[Int]): Gen[List[A]] = 
  	glen flatMap(n => this.listOfLength(n))
  // The book calls this `listOfN`

  // Ex 8.10 Implement helper functions for converting Gen to SGen.
  def unsized: SGen[A] = SGen(_ => this)    
}


//==== begin: Gen companion object ======================================
object Gen {
  // Ex 8.4 Implement Gen.choose which generates ints in the range [start, stopExclusive).
  def choose(start: Int, stopExclusive: Int): Gen[Int] = 
    Gen ( State(RNG.nonNegativeLessThan(stopExclusive -start)).map(_ + start))
    // RNG.nonNegativeLessThan returns a Rand[Int], which is an alias for RNG => (Int, RNG).
    // nonNegativeLessThan(b-a) gives a random int in [0, b-a).
    // map(_+a) take that int to the interval [a, b), as desired.
    // This is simpler than the official solution.

  // Ex 8.5 Let's see what else we can implement using this representation of Gen . 
  // Try implementing unit, boolean, and listOfN.
  def unit[A](a: => A): Gen[A] = Gen(State.unit[RNG,A](a)) // (Scala will infer the type of State.unit here.)

  def integer: Gen[Int] = Gen(State(RNG.int))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def double: Gen[Double] = Gen(State(RNG.double))
  
  // Generate a Gen[List[A]] by repeated use of the given generator `g`.
  // Length of list is given by input parameter `n` 
  def listOfLength[A](n: Int, g: Gen[A]): Gen[List[A]]= 
  	Gen(State.sequence(List.fill(n)(g.sample))) 
  // Here, List.fill(n)(g.sample) results in (g.sample, g.sample, ..., g.sample): List[State[S,A]].

  // Ex 8.12 Implement a listOf combinator that doesn't accept an explicit length. It should return an
  // SGen instead of a Gen. The implementation should generate lists of the requested size.
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfLength(n))
  
  // Ex 8.13 Define listOf1 for generating nonempty lists, and then update your 
  // specification of max to use this generator.
  def nonEmptyListOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfLength(n max 1)) 

  // Not the most efficient implementation, but it's simple.
  // This generates ASCII strings.
  def stringN(n: Int): Gen[String] =
    listOfLength(n, choose(0,127)).map(_.map(_.toChar).mkString)

  // This generates ASCII strings of random length.
  def stringGenN(g: Gen[Int]): Gen[String] =
    (choose(0,127).listOfGeneratedLength(g)).map(_.map(_.toChar).mkString)

  // generates strings of varying length
  val string: SGen[String] = SGen(stringN)
    
  // p.131: If we can generate a single Int in some range, do we need a new primitive to 
  // generate an (Int,Int) pair in some range? Answer: No, we don't need new primitives.
  // It's very easy to get a list of two random integers in the interval [a,b):
  def intListOfTwoInts(a: Int, b: Int): Gen[List[Int]] = listOfLength[Int](2, choose(a,b))  
  // It's only slightly harder to get a pair of two random integers in the interval [a,b):
  def intPair(a: Int, b: Int): Gen[(Int,Int)] = 
    Gen(listOfLength[Int](2, choose(a,b)).sample.map{ case List(x,y) => (x,y) })  

  // Generate triples of type A.
  def triple[A](g: Gen[A]): Gen[(A,A,A)] = 
    Gen(listOfLength[A](3, g).sample.map{ case List(x,y,z) => (x,y,z) })  

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

    
	///////////////////////////////////////////////////////
	//                                                   // 
  //       How to Generate Functions at Random         //
  //                                                   //
  ///////////////////////////////////////////////////////
  
	/* Ex 8.18 Come up with some other properties that takeWhile should satisfy. 
	 * Can you think of a property expressing the relationship between takeWhile and dropWhile?
	 * Solution:
	 * First, let's record the example given in the book:
	 * Ex 1. For all s: List[A] and all f: A => Boolean, the following evaluates to true:
	 *          s.takeWhile(f).forall(f)
	 * Here's my example:
	 * Ex 2. For all s: List[A] and all f: A => Boolean, the following evaluates to true:
	 *          s.takeWhile(f) append s.dropWhile(f) == s
	 */

	// We could take the approach of only examining particular arguments--i.e., specific 
	// higher-order functions. For instance, here's a more specific property for takeWhile:
	val isEven = (i: Int) => i%2 == 0 
 	val takeWhileProp =
 		Prop.forAll(Gen.listOf(Gen.integer))(ns => ns.takeWhile(isEven).forall(isEven))

	/* This works, but is there a way we could let the testing framework handle generating
	 * functions to use with takeWhile? To make this concrete, let's suppose we have a Gen[Int] 
	 * and would like to produce a Gen[String => Int]. What are some ways we could do that? 
	 * Well, we could produce String => Int functions that simply ignore their input string and 
	 * delegate to the underlying Gen[Int] :
	 */ 
 	def genStringIntConstFn(g: Gen[Int]): Gen[String => Int] =
 		g map (i => (s => i))
          
	/* This approach isn't sufficient though. We're simply generating constant functions that ignore 	
	 * their input. In the case of takeWhile, where we need a function that returns a Boolean, this 
	 * will be a function that always returns true or always returns false---clearly not very 
	 * interesting for testing the behavior of our function.
	 */

  /* Ex 8.19 (Hard) We want to generate a function that uses its argument in some way to select which
	 * Int to return. Can you think of a good way of expressing this? This is a very open-ended and 
	 * challenging design exercise. See what you can discover about this problem and if there's a nice 
	 * general solution that you can incorporate into the library we've developed so far.
	 * 
	 * _Solution_  One very simple solution that's a little better than the constant function 
	 * approach above would be to take as input a Gen[Int], as well as specific function mapping 
	 * String to Int, and then use both of these inputs to randomly generate a function; e.g., we 
	 * could multiply the return value of that function by the randomly generated Int, as follows:
	 */ 
	def genStringIntFn(g: Gen[Int])(f: String => Int): Gen[String => Int] =
		g map (i => (s => i*f(s)))

  // Here's an example use: 
 	val fn: Gen[String => Int] = genStringIntFn(choose(-10,10))(_.length)
	//(added type signature here just so Scala will complain if the type of fn is not as I expected.)
 
 	// Another approach suggested in the hint given in the textbook companion is similar.  
 	// We could set the seed of the random int generator equal to the hashcode of the given string.
 	// Slightly more general, instead of using hashcode, specify the function you want here: 
 	def h[A](a:A): Long = ???
 	def genFn[A,B](g: Gen[B]): Gen[A => B] = Gen {
	 	State { (rng: RNG) =>
	 			val (seed, rng2) = rng.nextInt
	 			val f = (a:A) => g.sample.run(RNG.Simple(seed.toLong ^ h(a)))._1
	 			(f, rng2)
	 	}
 	}

	// We could make a trait to abstract out the h function needed in the implementation above.
	// This is simple-minded/easy-to-understand. We'll generalize it later.

	trait Seeder[-A] {
		def seedFn : A => Long
	} 
 
	def genFn_from_seedFn[A,B](in: Seeder[A])(out: Gen[B]): Gen[A => B] = Gen {
	 	State { (rng: RNG) =>
	 			val (seed, rng2) = rng.nextInt
	 			val f = (a:A) => out.sample.run(RNG.Simple(seed.toLong ^ in.seedFn(a)))._1
	 			(f, rng2)
		 	}
 	}
 
	// Example use:
	val seeder = new Seeder[String]{
		def seedFn = (s:String) => s.hashCode.toLong
	}
	// This should give the same function generator as that discussed in the book companion.
	def genStringFn[B](g: Gen[B]): Gen[String => B] =	genFn_from_seedFn[String,B](seeder)(g)
	
	// And finally, for the more general version, 
	trait CoGen[-A] {
		def sample(a: A, rng: RNG): RNG
	}
	
	def fn[A,B](in: CoGen[A])(out: Gen[B]): Gen[A => B] = Gen {
		State { (rng: RNG) =>
			val (seed, rng2) = rng.nextInt
			val f = (a: A) => out.sample.run(in.sample(a, rng))._1
			(f,rng2) }
	}
	
} //==== end: Gen companion object ======================================


case class SGen[+A](forSize: Int => Gen[A]){

	def apply(n: Int): Gen[A] = forSize(n)
	
  // Ex 8.11 SGen supports many of the same ops as Gen. Define some convenience functions on SGen that 
  // simply delegate to the corresponding functions on Gen.
  def flatMap[B](f: A => SGen[B]): SGen[B] = 
  	SGen(n => this.forSize(n).flatMap { x => f(x).forSize(n) })
  // Not sure if flatMap is correct.  Better check it.
  
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




// =========vvvvvvvvvv MISCELLANEOUS NOTES AND COMMENTS vvvvvvvvvvv=========
/* Initially, the return type of a Prop was Option, with None returned in case of failure.
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


/* Chapter 8 is clear up to Section 8.3, where the authors discuss test case minimization.
 * They begin by referring to an abstract notion of size of a test case, and refer to the
 * "smallest" or "simplest" test case. Then they introduce a new case class
 * 
 *    case class SGen[+A](forSize: Int => Gen[A])
 *    
 * which has a function forSize that accepts a test "size" and produces a generator of tests of that size.
 * The problem is that the authors never clarify what size really means.        
 */

/* Old forAll code:
 * Use randomStream to get random stream of A's, then zip that with a stream of ints to get a stream of 
 * pairs of type (A, Int). Then, take(n) from the stream of pairs apply the map which tests the predicate.
 * 
 * def forAll[A](g: Int => Gen[A])(predicate: A => Boolean): Prop = Prop {
 *     (n, rng) => randomStream(g)(rng).zip(Stream.from(0)).take(n).map {
 *           case (a, i) => try {
 *             if (predicate(a)) Passed else Falsified(a.toString, i)
 *           } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
 *     }.find(_.isFalsified).getOrElse(Passed)
 * }
 * 
 * def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
 */


  /* It seems all these type aliases are obfuscating matters. Wouldn't it be easier if we simply let 
   * Gen wrap a function of type RNG => (A, RNG), rather than have it wrap a State that wraps a 
   * function of type RNG => (A, RNG)? The problem with this seemingly simpler solution is that it 
   * doesn't allow us to use the State class methods (like the map method we used above).
   */

  /* **Sec 8.2.4 Generators that depend on generated values**
   * Suppose we'd like a `Gen[(String,String)]` that generates pairs where the second string contains
   * only characters from the first. Or that we had a `Gen[Int]` that chooses an integer between 0 and 11, 
   * and we'd like to make a `Gen[List[Double]]` that then generates lists of whatever length is chosen. 
   * In both of these cases there's a dependency--we generate a value, and then use that value to determine 
   * what generator to use next. For this we need `flatMap`, which lets one generator depend on another.
   */


  /* Remarks on motivation for random generation of higher-order functions. As mentioned above, 
   * the purpose of forAll2 is to run forAll on generated A's *and* generated predicates. 
   * It might seem weird because, usually, we have a specific prediate in mind that
   * we're trying to test on lots of randomly generated data. However, consider the example on 
   * page 142. To test the takeWhile function, we want to check that for every list 
   * ls: List[A], and for every predicate f: A => Boolean, the expression
   *    ls.takeWhile(f).forall(f) 
   * results in true. In this case, a random generator of higher-order functions, like 
   * f: A => Boolean, is precisely what we need.
   */
