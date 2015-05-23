package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(t1, t2) => 1 + size(t1) + size(t2)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(t1, t2) => maximum(t1) max maximum(t2)
  } 
  
  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
      case Leaf(v) => 1
      case Branch(t1, t2) => 1+ (depth(t1) max depth(t2))
  }  

  def depth_alternative[A](t: Tree[A]): Int = {
    def depth_aux[A](tr: Tree[A], maxdepth: Int): Int = tr match {
      case Leaf(v) => 1+maxdepth
      case Branch(t1, t2) => depth_aux(t1, maxdepth+1) max depth_aux(t2, maxdepth+1)
    }
    depth_aux(t, 0)
  }
  
  // Exercise 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(t1, t2) => Branch(map(t1)(f), map(t2)(f))
  }

  // Exercise 2.29: foldLeft on binary trees
  def foldLeft[A,B](t: Tree[A], b: B) (f: (B,A) => B): B = t match {
    case Leaf(a) => f(b,a)
    case Branch(t1, t2) => foldLeft(t2,foldLeft(t1, b)(f))(f)
  }
  
  def size_with_fold[A](t: Tree[A]): Int = foldLeft(t, 1)((n,a) => n + 1)

}