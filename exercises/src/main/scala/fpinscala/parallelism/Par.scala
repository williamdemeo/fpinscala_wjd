/* File: Par.scala (Ch 7)
 * Authors: Paul Chiusano and Runar Bjarnason
 * Url: https://github.com/fpinscala/fpinscala 
 * 
 * Description: This is a modified version of the file Par.scala
 *   that accompanies the book "Functional Programming in Scala" by
 *   Chiusano and Bjarnason. This version of the file includes 
 *   solutions to some of the exercises in 
 * 
 *     CHAPTER 7: Purely functional parallelism 
 * 
 *   The solutions herein are by William DeMeo <williamdemeo@gmail.com>.
 *   They are at best imperfect, and possibly wrong.  Official solutions by 
 *   Chiusano and Bjarnason are available in the github repo mentioned above.
 */

package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // `unit` marks its argument as the type of expression that might be involved in a parallel computation.  
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) 
  /* `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation 
   * of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. 
   * It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
   */

  // `fork` takes an expression that has been marked for parallel computation and further declares
  // that it should be evaluated in a separate logical thread.
  def fork[A](a: => Par[A]): Par[A] = 
    es => es.submit( new Callable[A] { def call = a(es).get } )
  /* This is the simplest and most natural implementation of `fork`, but there are some problems 
   * with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. 
   * Since this blocking occupies a thread in our thread pool, or whatever resource backs the 
   * `ExecutorService`, this implies that we're losing out on some potential parallelism. 
   * Essentially, we're using two threads when one should suffice. This is a symptom of a more 
   * serious problem with the implementation, and we will discuss this later.
   */

  // `lazyUnit` converts its argument to a Par[A] and then "p thunks" it. 
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  /* To be honest, I don't really see the point of lazyUnit. Isn't a UnitFuture "always done"?
   * Why then mark it for (delayed) parallel computation in a separate logical thread with fork?
   * (But see the asyncF method below.)
   */
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  // Ex 7.1 Par.map2 is a new higher-order function for combining the result of two parallel computations.
  // What is its signature? Give the most general signature possible (don't assume it works only for Int).
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    /* Notes: `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design 
     * choice of having `fork` be the sole function in the API for controlling parallelism. We can always 
     * do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
     *
     * This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. 
     * This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait.
     * It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`,
     * applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` 
     * implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available 
     * time allocated for evaluating `bf`.
     */
  }
      
  // Ex 7.2 Before continuing, try to come up with representations for Par that make it possible to implement the functions of our API.
    
  // Ex 7.3 (Hard) Fix the implementation of map2 so that it respects the contract of timeouts on Future.    
  // (skipping for now)
    
  // Ex 7.4 This API already enables a rich set of operations. Here's a simple example: using lazyUnit, write a function to 
  // convert any function A => B to one that evaluates its result asynchronously.
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  /* From page 108 of fpinscala...
   *   Suppose we have a Par[List[Int]] representing a parallel computation that produces a List[Int], 
   *   and we'd like to convert this to a Par[List[Int]] whose result is sorted. We could of course run the Par, 
   *   sort the resulting list, and repackage it in a Par with unit.  But we want to avoid calling run. 
   *   The only other combinator we have that allows us to manipulate the value of a Par in any way is map2. 
   *   So if we passed parList to one side of map2, we'd be able to gain access to the List inside and sort it. 
   *   And we can pass whatever we want to the other side of map2, so let's just pass a no-op: 
   */
  def sortPar_first_try(parList: Par[List[Int]]): Par[List[Int]] = 
    map2(parList, unit(()))((a,_) => a.sorted)

  // Let's generalize this to a method that lifts a function of type A => B to a function of type Par[A] => Par[B]
  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  // Now sortPar is easier and more readable.
  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // Ex 7.5 (Hard) Write a function called sequence with the given signature
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = 
    ps.foldRight[Par[List[A]]](unit(List()))((x,y) => map2(x,y)( _::_ ))
  // (Same as sequence method in Option.scala.)
    
  // Ex 7.5b Implement a parMap method that maps over a list in parallel. Use existing combinators. 
  // Unlike map2, which combines two parallel computations, parMap needs to combine N parallel computations.
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork{
    // First construct a list of Par[A]'s using List[A]'s map method.
    val fps: List[Par[B]] = ps.map(asyncF(f)) // a list of parallel computations
    // asyncF takes A => B to A => Par[B]. Now we just collect the results of these
    // par comps as a Par[List[B]] using sequence:
    sequence(fps)
    /* We wrap it all in a call to fork, so when we later call run, this will fork a single 
     * asynchronous computation spawning N parallel computations, wait for these 
     * computations to finish, and collect their results in a list.
     */
  }
  
  // Ex 7.6 Implement parFilter, which filters elements of a list in parallel.
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pas: List[Par[List[A]]] = as.map(asyncF((a:A) => if(f(a)) List(a) else List()))
    map(sequence(pas))(_.flatten)
  } // (copied official solution)
  
  // Leibniz equality: 
  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get
  // (two expressions are "Leibniz equal" if they evaluate to the same value no matter the context)

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
          else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
