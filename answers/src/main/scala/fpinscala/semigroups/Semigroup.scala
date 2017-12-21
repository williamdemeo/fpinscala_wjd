package fpinscala.semigroups

trait Semigroup[A] {
  def op(a1: A, a2: A): A
}

object Semigroup {

}