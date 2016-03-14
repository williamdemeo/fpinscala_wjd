// Authors: Paul Chiusano and Runar Bjarnason
// Url: https://github.com/fpinscala/fpinscala 
// 
// Exercises in this file were solved by williamdemeo@gmail.com.
// The solutions may be imperfect. Authoritative solutions are provided by 
// Chiusano and Bjarnason at the github repository cited above.

package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
  } 
  
  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1+ (depth(left) max depth(right))
  }  

  def depth_alternative[A](t: Tree[A]): Int = {
    def depth_aux[A](tr: Tree[A], maxdepth: Int): Int = tr match {
      case Leaf(_) => 1+maxdepth
      case Branch(left, right) => depth_aux(left, maxdepth+1) max depth_aux(right, maxdepth+1)
    }
    depth_aux(t, 0)
  }
  
  // Exercise 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Exercise 3.29: foldLeft on binary trees
  /* Recall that, in general, a fold operation for a given type receives a "handler" for each of the data 
   * constructors of the type, and recursively accumulates some value using these handlers. 
   * For trees, the invariant foldLeft(t)(Leaf(_))(Branch(_,_)) == t should hold, and we can use this 
   * function to implement just about any recursive function that would otherwise be defined by pattern 
   * matching. */
  def foldLeft[A,B](t: Tree[A], z: B)(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(foldLeft(left, z)(f)(g),foldLeft(right, z)(f)(g))
  }
  
  def size_with_foldLeft[A](t: Tree[A]): Int = foldLeft(t, 0)(_ => 1)(_ + _ + 1)

}