/* File: State.scala (Ch 6)
 * Authors: Paul Chiusano and Runar Bjarnason
 * Url: https://github.com/fpinscala/fpinscala 
 * 
 * Description: This is a modified version of the file State.scala
 *   that accompanies the book "Functional Programming in Scala" by
 *   Chiusano and Bjarnason. This version of the file includes 
 *   solutions to some of the exercises in 
 * 
 *     CHAPTER 6: Purely functional state
 * 
 *   The solutions herein are by William DeMeo <williamdemeo@gmail.com>.
 *   They are at best imperfect, and possibly wrong.  Official solutions by 
 *   Chiusano and Bjarnason are available in the github repo mentioned above.
 */

package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. 
}

object RNG {

	// NB - this was called SimpleRNG in the book text
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  /* We can think of a value of type Rand[A] as "a randomly generated A," although that's not really precise. 
   * It's really a state action--a program that depends on some RNG, uses it to generate an A, and also 
   * transitions the RNG to a new state that can be used by another action later. We can now turn methods 
   * such as RNG's nextInt into values of this new type:
   */
  val int: Rand[Int] = _.nextInt

  /* We want to write combinators that let us combine Rand actions while avoiding explicitly passing along 
   * the RNG state. We'll end up with a kind of domain-specific language that does all of the passing for us.
   * For example:
   */
  // The unit action---a simple RNG state transition which passes the RNG state through without 
  // using it, always returning a constant value rather than a random value.
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // The map action--transforms the output of a state action without modifying the state itself. 
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  	val (a, rng2) = s(rng) 
  	(f(a), rng2)
  }
  // Rand[A] is an type alias for RNG => (A, RNG), so map is just a function composition.
  
  /* Ex 6.1 Write a function that uses RNG.nextInt to generate a random integer 
   * between 0 and Int.maxValue (inclusive). Make sure to handle the corner case 
   * when nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart.
	 */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
  	val (i,r) = rng.nextInt 
  	if (i<0) (-(i+1), r ) else (i, r)
  }

  /* Ex 6.2 Write a function to generate a Double between 0 and 1, not including 1. 
   * Note: You can use Int.MaxValue to obtain the maximum positive integer value, 
   * and you can use x.toDouble to convert an x: Int to a Double.
   */
  def double(rng: RNG): (Double, RNG) = {
  	val (i,r) = nonNegativeInt(rng)
  	(i/(Int.MaxValue.toDouble+1), r) 
  } // checked (okay, but minor fix needed: add 1 to denominator)

  /* Ex 6.3 Write functions to generate an (Int, Double) pair, a (Double, Int) pair, 
   * and a (Double, Double, Double) 3-tuple. You should be able to reuse the functions 
   * you've already written.
   */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
  	val (i, r1) = rng.nextInt
  	val (d, r2) = double(r1)
  	((i, d), r2)
  } // checked (same as official solution)
  
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
  	val ((i,d),r) = intDouble(rng)
  	((d,i),r)
  } // checked (same as official solution)
  
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
  	val (d1,r1) = double(rng) 
  	val (d2,r2) = double(r1) 
  	val (d3,r3) = double(r2)
  	((d1,d2,d3),r2)
  } // checked (same as official solution)

	// Ex 6.4 Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  	@annotation.tailrec
  	def ints_aux(my_count: Int, my_rng: RNG, acc: List[Int]): (List[Int], RNG) = 
  		my_count match {
  			case n if (n <= 0) => (acc, rng)
  			case n => {
	  			val (i,r) = my_rng.nextInt
  				ints_aux(n-1, r, i::acc)
  			}
  	}
  	ints_aux(count, rng, Nil)
  }  // checked (similar to official solution)

  // Ex 6.5 Use map to reimplement double in a more elegant way. See exercise 6.2.
  def double_with_map(rng: RNG): (Double, RNG) = 
  	map(nonNegativeInt)(_/(Int.MaxValue.toDouble+1))(rng)
	// checked (same as official solution)
  	
	/* Ex 6.6 Write the implementation of map2 based on the following signature. 
	 * This function takes two actions, ra and rb, and a function f for combining 
	 * their results, and returns a new action that combines them:
	 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
  	rng => {
  		val (a, rng1) = ra(rng)
  		val (b, rng2) = rb(rng1)
  		(f(a,b),rng2)
  } // checked (same as official solution) 

  /* Ex 6.7 (Hard) If you can combine two RNG transitions, you should be able to 
   * combine a whole list of them. Implement sequence for combining a List of 
   * transitions into a single transition. Use it to reimplement the ints function 
   * you wrote before. For the latter, you can use the standard library function 
   * List.fill(n)(x) to make a list with x repeated n times. 
   */
 	// pm version 
  def sequence_first_try[A](fs: List[Rand[A]]): Rand[List[A]] = 
  	rng => {
	  	def sequence_aux(my_fs: List[Rand[A]], acc: List[A])(r: RNG): (List[A],RNG) =
	  		my_fs match {
	  		case fh::ft => {
	  			val (a, r1) = fh(r)
	  			sequence_aux(ft, a::acc)(r1)
	  			}
	  		case List() => (acc, r)
	  	}
	  	sequence_aux(fs,List())(rng)
  } // It's not pretty, but I think it works.
  // official solution:  	
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  	fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  /* Ex 6.1 Write a function that uses RNG.nextInt to generate a random integer 
   * between 0 and Int.maxValue (inclusive). Make sure to handle the corner case 
   * when nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart.
	 */

  /* There are some functions that we can't very well write in terms of map and map2.
   * E.g., nonNegativeLessThan, which generates an integer k in the range 0 <= k < n.
   * A first stab implementation is to generate a non-negative integer modulo n:
   */
  def nonNegativeLessThan_first_try(n: Int): Rand[Int] = map(nonNegativeInt){ _ % n }
  /* This will certainly generate a number in the range, but it'll be skewed because 
   * Int.MaxValue may not be exactly divisible by n. So numbers that are less than the
   * remainder of that division will come up more frequently.
   */

  def nonNegativeLessThan_second_try(n: Int): Rand[Int] = rng => { 
      val(i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n-1) - mod >= 0) (mod,rng2) 
      else nonNegativeLessThan_second_try(n)(rng2)
  }
  // Maybe we should have another combinator that takes care of the passing along of rng2...
  // ...indeed, this is what flatMap does.

  // Ex 6.8 Implement flatMap, and then use it to implement nonNegativeLessThan.
	def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
		rng => {
			val (a, r) = f(rng)
			g(a)(r)
		} // NB  g(a)(r) has type (B, RNG), which is what we want since Rand[B] 
		// is an alias for type `RNG => (B, RNG)` (and we already have `rng => {` above)

	def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) {
	       i => if (i + (n-1) - (i%n) >= 0) unit(i%n) else nonNegativeLessThan(n)
	} // checked (this is correct, but seems no less awkward than the second try above)
	
  /* flatMap allows us to generate a random A with Rand[A], and then take that A 
   * and choose a Rand[B] based on its value. In nonNegativeLessThan, we use it to 
   * choose whether to retry or not, based on the value generated by nonNegativeInt.
   */

	// Ex 6.9 Reimplement map and map2 in terms of flatMap. The fact that this is possible indicates 
	// that flatMap is in some sense more "powerful" than map and map2.
	def map_via_flatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = 
		flatMap(s)(a => (rng => (f(a),rng)))
	// NB `(rng => (f(a),rng))` is the same as unit(f(a)). Otherwise, this is the same as official solutions.
		
	def map2_via_flatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
		flatMap(ra)(a => map(rb)(b => f(a,b)))
		// checked (same as official solutions)
	 
	/* We can now revisit our example from the beginning of the chapter. 
	 * Can we make a more testable die roll using our purely functional API?
	 * Here's an implementation of rollDie using nonNegativeLessThan, including the
	 * off-by-one error we had before.
	 */
  def rollDie_first_try: Rand[Int] = nonNegativeLessThan(6)
  /* If we test this function with various RNG states, we'll pretty soon find an 
   * RNG that causes this function to return 0. 
   * (Test it in Ch06_state.sc worksheet with `val zero = rollDie(SimpleRNG(5))._1`)
	 * And we can re-create this reliably by using the same SimpleRNG(5) random generator, 
	 * without having to worry that its state is destroyed after it's been used.
	 * Fixing the bug is trivial:
	 */
  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
  
}

/* The functions map, map2, flatMap, and sequence above aren't specific to random number 
 * generation. They're general-purpose functions for working with state actions, and don't 
 * care about the type of the state. For instance, map doesn't care that it's dealing with 
 * RNG state actions, and we can give it a more general signature:
 *     def map[S,A,B](a: S => (A,S))(f: A => B): S => (B,S)
 * Changing this signature doesn't require modifying the implementation of map!
 * We should come up with a more general type than Rand for handling any type of state:  
 */

// Import the State companion object defined below so we can use, e.g., State.unit.
import State._  

/* We should now come up with a more general type than Rand, for handling any type of state.
 * We could use a simple type alias, like
 * 
 *    trait State[S,+A] = S => (A,S)
 * 
 * Here State is short for computation that carries some state along, or state action, state transition,
 * or even statement. Alternatively, we could make a new class that wraps the underlying function:
 */
case class State[S,+A](run: S => (A, S)) {
  
/* Ex 6.10 Above we defined the following functions for the class RNG:
 *
 *     def unit[A](a: A): Rand[A]  
 *     def map[A,B](s: Rand[A])(f: A => B): Rand[B]
 *     def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]
 *     def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B]
 *     def sequence[A](fs: List[Rand[A]]): Rand[List[A]]
 * 
 * From the names and type signatures, we can tell what these functions are meant to do, and   
 * they all produce a Rand[X] for the given type X. Recall that Rand[X] is simply a type alias:
 *  
 *     type Rand[A] = RNG => (A, RNG).
 *  
 * So all of the above methods return a function from RNG to (A, RNG); i.e., a function that takes
 * a "state" s1 and returns a value along with a new state (a, s2).  
 * 
 * Generalize the above list of functions making them methods of the State case class where 
 * possible. Otherwise, put them in a State companion object.
 */
  
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s1 => {
    val (a, s2) = this.run(s1)
    f(a).run(s2)
    })
  def map_first_try[B](f: A => B): State[S, B] = State(s1 => { 
    val (a, s2) = this.run(s1)
    (f(a),s2)
    })
  def map_second_try[B](f: A => B): State[S, B] = flatMap(a => (State(s => (f(a), s))))
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a))) 
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
  
  // I'm not sure why unit must go in State companion object and not right here.
  //     def unit[S,A](a: A): State[S, A] = State(s => (a,s))
  // But if we put it here, it seems we can't use it in the companion object...?
}
/* The representation (type alias or class) doesn't matter too much. What's important is that we have 
 * a single, general-purpose type, and using this type we can write general-purpose functions for capturing 
 * common patterns of stateful programs. And we can now just make Rand a type alias for State: 
 *     type Rand[A] = State[RNG, A]
 * (We do this inside the State companion object below.)
 */

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  // sequence--converts a list of stateful things to a stateful list of things.
  def sequence[S,A](sas: List[State[S,A]]): State[S,List[A]] =
  	sas.foldRight(unit[S, List[A]](List()))((s, acc) => s.map2(acc)(_ :: _))
  
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
