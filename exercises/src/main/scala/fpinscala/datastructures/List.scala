package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`

case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists.
// Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  // Exercise 3.2: 
  // Implement the function `tail` for removing the first element of a list    
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Error("tail called on empty list")
    case Cons(h, t) => t
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(oldh, t) => Cons(h, t) 
  } 
    
  // Exercise 3.4: Drop the first n elements of a list; return the rest.
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n==0) l else l match {
      case Nil => throw new Error("drop called on empty list")
      case Cons(h, t) => drop(t, n-1)
    }
  }

  // Exercise 3.5: dropWhile
  // Drop each leading element h that satisfies f(h).
  // As soon as an element appears that doesn't satisfy f, return the remaining list.  
  // (grouping arguments allows for better type inference)
  def dropWhile[A] (l: List[A]) (f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f) 
    case _ => l
  }

  // Exercise 3.6: init
  // Take all but the last element of a list, dropping the last.
  // (non-tail-recursive version)
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Error("init called on empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) =>  Cons(h, init(t))
  }
  
  // Exercise 3.6: init_tr
  // Take all but the last element of a list, dropping the last.
  // (tail-recursive version)
  def init_tr[A](l: List[A]): List[A] = {
    def init_aux(xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => throw new Error("init called on empty list")
      case Cons(h, Nil) => acc
      case Cons(h, t) => init_aux(t, append(acc, Cons(h,Nil)))
    }
    init_aux(l, Nil)
  }

  // length: return the (integer) length of a list  
  // (non-tail-recursive version)
  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(h, t) => 1 + length(t)
  }

  // length_tr: return the (integer) length of a list  
  // (tail-recursive version)
  def length_tr[A](l: List[A]): Int = {
    def length_aux[A](l: List[A], acc: Int): Int = l match {
      case Nil => acc
      case Cons(h, t) => length_aux(t, acc+1)
    }
    length_aux(l,0)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = sys.error("todo")

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}
