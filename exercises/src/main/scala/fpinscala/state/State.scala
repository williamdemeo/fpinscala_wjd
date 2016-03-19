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

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  	val (a, rng2) = s(rng) 
  	(f(a), rng2)
  }

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

  // Ex 6.8 Implement flatMap, and then use it to implement nonNegativeLessThan.
	def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
		rng => {
			val (a, r) = f(rng)
			g(a)(r)
		} // NB  g(a)(r) has type (B, RNG), which is what we want since Rand[B] 
		// is an alias for type `RNG => (B, RNG)` (and we already have `rng => {` above)

  /* flatMap allows us to generate a random A with Rand[A], and then take that A 
   * and choose a Rand[B] based on its value. In nonNegativeLessThan, we use it to 
   * choose whether to retry or not, based on the value generated by nonNegativeInt.
   */

	/* Ex 6.9 Reimplement map and map2 in terms of flatMap. The fact that this is 
	 * possible is what we're referring to when we say that flatMap is more powerful 
	 * than map and map2.
	 */
	def map_via_flatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = 
		flatMap(s)(a => (rng => (f(a),rng)))
	// NB `(rng => (f(a),rng))` is the same as unit(f(a))
	// otherwise, this is the same as official solutions
		
	def map2_via_flatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
		flatMap(ra)(a => map(rb)(b => f(a,b)))
		// checked (same as official solutions)
	 
	/* Ex 6.10 Generalize the functions unit, map, map2, flatMap, and sequence. 
	 * Add them as methods on the State case class where possible. Otherwise you 
	 * should put them in a State companion object.
	 */
  // Skipping Ex 6.10 for now.  Maybe come back to it later.
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
