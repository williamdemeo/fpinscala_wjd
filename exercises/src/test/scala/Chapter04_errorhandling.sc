package fpinscala.errorhandling
// import Option._

//hide std library `Option` and `Either`, since we are writing our own in this chapter

object Chapter04_scratch {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
/** __Option composition, lifting, and wrapping exception-oriented APIs__

It may be easy to jump to the conclusion that once we start using Option, it infects our
entire code base. One can imagine how any callers of methods that take or return
Option will have to be modified to handle either Some or None . But this doesn't happen,
and the reason is that we can lift ordinary functions to become functions that operate
on Option... cool!

For example, the map function lets us operate on values of type Option[A] using a
function of type A => B, returning Option[B]. Another way of looking at this is that map
turns f : A => B into a function of type Option[A] => Option[B]. Let's
make this explicit:
**/

def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
                                                  //> lift: [A, B](f: A => B)fpinscala.errorhandling.Option[A] => fpinscala.errorh
                                                  //| andling.Option[B]

// Here's how we "lift" the usual abs: Double => Double function.
val absO: Option[Double] => Option[Double] = lift(math.abs)
                                                  //> absO  : fpinscala.errorhandling.Option[Double] => fpinscala.errorhandling.O
                                                  //| ption[Double] = <function1>

// Let's think of ways to use the lift monad for universal algebra applications.
// Equivalence relation trait.
// This will be a reflexive, symmetric, transitive subset of A x A.
// We could also view an equivalence relation R as a function
// R: A x A => Boolean, where R(x,y) = 1 iff (x,y) in R.
// Currying this, we have R: A => A => Boolean.
// Properties:
//   1. R(x)(x) = 1 for all x
//   2. R(x)(y) = 1 ==> R(y)(x) = 1
//   3. R(x)(y) = 1 and R(y)(z) = 1 ==> R(x)(z) = 1

/**
*Thunk*
We can pass an argument (say, `expr`) to a function using call-by-name
instead of call-by-value, so that `expr` isn't automatically evaluated.
Instead, evaluation of `expr` is delayed until we need it.  Suppose,
for example, we want to pass some expression that will evaluate to
an Int. Instead of specifying the signature of our function to accept
an Int argument, we will specify that it accepts a function argument of
type () => Int.  The result is that the thing passed in will be
evaluated using call-by-name instead of call-by-value.
*/
// Example

def maybeTwice(b: Boolean, delayedInt: () => Int) = if(b) delayedInt() + delayedInt() else 0
                                                  //> maybeTwice: (b: Boolean, delayedInt: () => Int)Int

// If we call this function as follows,

val x = maybeTwice(true, () => { println("hi (twice)"); 1+41 } )
                                                  //> hi (twice)
                                                  //| hi (twice)
                                                  //| x  : Int = 84
/**
In general, the unevaluated form of an expression is called a *thunk*.

This has an analog in universal algebra.  We often want to single out
special elements of the universe and include them in the set of basic
operations of the algebra. This is done by identifying such elements with
constant, or "nullary", functions.  For example, in an additive group,
(A, {+, -, 0}), we have the binary addition operation +: A x A => A,
the unary inverse operation -: A => A, and the additive identity element
0 is a nullary operation, 0: () => A. Thus, the additive identity plays a
dual role. It is both an element of the universe and a basic operation,
specifically a unary operation, which is a function of type () => A.

In Scala, this is so common that there is simpler syntax for it. We can
leave out the empty parens: */

def maybeTwice_sugared(b: Boolean, delayedInt: => Int) = if(b) delayedInt + delayedInt else 0
                                                  //> maybeTwice_sugared: (b: Boolean, delayedInt: => Int)Int

// When we call the function, we also drop `() =>`:

val y = maybeTwice_sugared(true, { println("hi (twice)"); 1+41 } )
                                                  //> hi (twice)
                                                  //| hi (twice)
                                                  //| y  : Int = 84



/** Sometimes we want to delay evaluation, but we also don't want to
(inefficiently) reevaluate the expression each time we need it. For this
we use the `lazy` keyword: */

def maybeTwice_smarter(b: Boolean, delayedInt: => Int) = {
 	if (b) {
 		lazy val j = delayedInt
 		j+j
 	}
 	else 0
 }                                                //> maybeTwice_smarter: (b: Boolean, delayedInt: => Int)Int
	
val z = maybeTwice_smarter(true, { println("hi (once)"); 1+41 } )
                                                  //> hi (once)
                                                  //| z  : Int = 84

}