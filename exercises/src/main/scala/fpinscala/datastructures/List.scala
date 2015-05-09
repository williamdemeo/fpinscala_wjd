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


  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)  // `_ + _` is more concise notation for `(x,y) => x + y`

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`


  // Exercise 3.2: 
  // Implement the function `tail` for removing the first element of a list    
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Error("tail called on empty list")
    case Cons(h, t) => t
  }

  // Exercise 3.3: replace the first element of a list with a new value
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

  // Exercise 3.9: return the (integer) length of a list using foldRight
  def length[A](l: List[A]): Int = foldRight(l, 0)((x,y) => 1+y) 

  // length: return the (integer) length of a list (non-tail-recursive version)
  def length_non_tr[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(h, t) => 1 + length_non_tr(t)
  }

  // length_tr: return the (integer) length of a list (tail-recursive version) 
  // but see also foldRight version (Exercise 3.9)
  def length_tr[A](l: List[A]): Int = {
    def length_aux[A](l: List[A], acc: Int): Int = l match {
      case Nil => acc
      case Cons(h, t) => length_aux(t, acc+1)
    }
    length_aux(l,0)
  }


  // Exercise 3.10: foldLeft (is a tail recursive fold operation)  
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // Exercise 3.11: write sum, product, and length using foldLeft
  def sum_fl(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def product_fl(l: List[Double]) = foldLeft(l, 1.0)(_ * _)
  def length_fl[A](l: List[A]): Int = foldLeft(l, 0)((x,y) => x+1) 

  // Exercise 3.12: reverse of a list
  def reverse_first_try[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append(reverse_first_try(t), List(h))
  } 

  // Exercise 3.12 (cont): in using foldLeft here, the type of Nil is required, 
  // otherwise Scala infers the B type parameter as List[Nothing]
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((h,t) => Cons(t, h))

  def foldRight_first_try[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercise 3.13: implement foldRight in terms of foldLeft (so it's tail recursive)
  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = foldLeft(reverse(l),z)((x,y) => f(y,x))
  
  // concatenate (aka append) list a2 to the end of list a1.
  def concat[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, concat(t, a2))
    }

  // Exercise 3.14: implement append (aka concatenate) in terms of foldRight.
  // append list a2 to the end of list a1.
  def append[A](a1: List[A], a2: List[A]): List[A] = 
    foldRight(a1,a2)(Cons(_,_))
    
  // Exercise 3.15: write a function that concatenates a list of lists into a single list.
  // Its runtime should be linear in the total length of all lists.
  def flatten[A](ll: List[List[A]]): List[A] = foldRight(ll, Nil:List[A])(append(_,_))

  // Exercise 3.17: write a function that turns each value in a List[Double] into a String.
  
  // Exercise 3.18: write a function `map` that generalizes modifying each element in a list while 
  // maintaining the structure of the list.
  def map_first_try[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map_first_try(t)(f))
  }
  // tail recursive version
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((x,y) => Cons(f(x),y))

  // Exercise 3.19: write a function `filter` that removes elements from a list unless they 
  // satisfy a given predicate.  use it to remove all odd numbers from a List[Int]
  def filter_first_try[A](l: List[A])(P: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (P(h)) Cons(h, filter_first_try(t)(P)) else filter_first_try(t)(P)
  }
  // tail recursive version
  def filter[A](l: List[A])(P: A => Boolean): List[A] = 
    foldRight(l, Nil:List[A])((x,y) => if(P(x)) Cons(x,y) else y)

  // Exercise 3.20: write a function `flatMap` that works like `map` except the function
  // given will return a list and that list should be inserted into the returned list.
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, Nil:List[B])((x,y) => append(f(x),y))
  
  // Exercise 3.21: Use `flatMap` to implement `filter`.
  def filter_with_flatMap[A](l: List[A])(P: A => Boolean): List[A] = 
    flatMap(l)(x => if(P(x)) List[A](x) else Nil:List[A])    
 
  // Exercise 3.23: Write a function `zipWith` that takes two lists and a binary function 
  // and returns the list consisting of the result of applying the function to each pair of 
  // elements from the lists.
  def zipWith_first_try[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah,bh), zipWith_first_try(at, bt)(f))
  }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    def zipWith_rev(a: List[A], b: List[B], c: List[C])(f: (A, B) => C): List[C] =
      (a, b) match {
        case (_, Nil) => c
        case (Nil, _) => c
        case (Cons(ah, at), Cons(bh, bt)) => zipWith_rev(at, bt, Cons(f(ah,bh), c))(f)
    }
    reverse(zipWith_rev(a, b, Nil)(f))
    
  }  
  
    
    
    
    
    
    
    
    
    
    
    
    
    
    
}
