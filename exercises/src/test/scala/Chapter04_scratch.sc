package fpinscala.errorhandling
// import Option._

//hide std library `Option` and `Either`, since we are writing our own in this chapter

object Chapter04_scratch {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
/** __Option composition, lifting, and wrapping exception-oriented APIs__

It may be easy to jump to the conclusion that once we start using Option, it infects our
entire code base. One can imagine how any callers of methods that take or return
Option will have to be modified to handle either Some or None . But this doesn't hap-
pen, and the reason is that we can lift ordinary functions to become functions that
operate on Option... cool!

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
}