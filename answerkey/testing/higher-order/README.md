Below is the book's answer to the open-ended Exercise 8.19 about 
randomly generating higher-order functions.  I've put this here
so that GitHub will render the markdown (because this topic is 
important to me).

### Testing higher-order functions
So far, our library seems quite expressive, but there's one area where it's lacking: we
don't currently have a good way to test higher-order functions. While we have lots of
ways of generating data using our generators, we don't really have a good way of
generating functions. 

For instance, let's consider the takeWhile function defined for `List` and `Stream`.
Recall that this function returns the longest prefix of its input whose elements
all satisfy a predicate. For instance, `List(1,2,3).takeWhile(_ < 3)` results in 
`List(1,2)`. A simple property we'd like to check is that for any list, 
`s: List[A]`, and any `f: A => Boolean`, the expression `s.takeWhile(f).forall(f)` 
evaluates to `true`. That is, every element in the returned list satisfies the
predicate. 

We could certainly take the approach of only examining particular arguments when
testing higher-order functions. For instance, here's a more specific property for
`takeWhile`:

~~~ Scala
val isEven = (i: Int) => i%2 == 0
val takeWhileProp =
  Prop.forAll(Gen.listOf(int))(ns => ns.takeWhile(isEven).forall(isEven))
~~~

This works, but is there a way we could let the testing framework handle generating
functions to use with `takeWhile`?  Let's consider our options. To make this concrete,
let's suppose we have a `Gen[Int]` and would like to produce a `Gen[String => Int]`.
What are some ways we could do that? Well, we could produce `String => Int`
functions that simply ignore their input string and delegate to the underlying 
`Gen[Int]`: 

~~~ Scala
def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
  g map (i => (s => i))
~~~

This approach isn't sufficient though. We're simply generating constant functions that
ignore their input. In the case of `takeWhile`, where we need a function that returns a
`Boolean`, this will be a function that always returns true or always returns
false---clearly not very interesting for testing the behavior of our function.

**EXERCISE 8.19**
Hard: We want to generate a function that uses its argument in some way to select which
Int to return. Can you think of a good way of expressing this? This is a very open-
ended and challenging design exercise. See what you can discover about this problem
and if there's a nice general solution that you can incorporate into the library we've
developed so far.

### Solution to Ex 8.19

Let's start by looking at the signature of our motivating example, generating a function from `String => Int` given a `Gen[Int]`:

~~~ Scala
def genStringInt(g: Gen[Int]): Gen[String => Int]
~~~

And let's generalize this a bit to not be specialized to `Int`, because that would let us cheat a bit (by, say, returning the `hashCode` of the input `String`, which just so happens to be an `Int`).

~~~ Scala
def genStringFn[A](g: Gen[A]): Gen[String => A]
~~~

We've already ruled out just returning a function that ignores the input `String`, since that's not very interesting! Instead, we want to make sure we _use information from_ the input `String` to influence what `A` we generate. How can we do that? Well, the only way we can have any influence on what value a `Gen` produces is to modify the `RNG` value it receives as input:

Recall our definition of `Gen`:

~~~ Scala
case class Gen[+A](sample: State[RNG,A])
~~~

Just by following the types, we can start writing:

~~~ Scala
def genStringFn[A](g: Gen[A]): Gen[String => A] = Gen {
  State { (rng: RNG) => ??? }
}
~~~

Where `???` has to be of type `(String => A, RNG)`, and moreover, we want the `String` to somehow affect what `A` is generated. We do that by modifying the seed of the `RNG` before passing it to the `Gen[A]` sample function. A simple way of doing this is to compute the hash of the input string, and mix this into the `RNG` state before using it to produce an `A`: 

~~~ Scala
def genStringFn[A](g: Gen[A]): Gen[String => A] = Gen {
  State { (rng: RNG) =>
    val (seed, rng2) = rng.nextInt // we still use `rng` to produce a seed, so we get a new function each time
    val f = (s: String) => g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
    (f, rng2)
  }
}
~~~

More generally, any function which takes a `String` and an `RNG` and produces a new `RNG` could be used. Here, we're computing the `hashCode` of the `String` and then XOR'ing it with a seed value to produce a new `RNG`. We could just as easily take the length of the `String` and use this value to perturn our RNG state, or take the first 3 characters of the string. The choices affect what sort of function we are producing:

* If we use `hashCode` to perturb the `RNG` state, the function we are generating uses all the information of the `String` to influence the `A` value generated. Only input strings that share the same `hashCode` are guaranteed to produce the same `A`.
* If we use the `length`, the function we are generating is using only some of the information of the `String` to influence the `A` being generated. For all input strings that have the same length, we are guaranteed to get the same `A`.

The strategy we pick depends on what functions we think are realistic for our tests. Do we want functions that use all available information to produce a result, or are we more interested in functions that use only bits and pieces of their input? We can wrap the policy up in a `trait`:

~~~ Scala
trait Cogen[-A] {
  def sample(a: A, rng: RNG): RNG
}
~~~

As an exercise, try implementing a generalized version of `genStringFn`.

~~~ Scala
def fn[A,B](in: Cogen[A])(out: Gen[B]): Gen[A => B]
~~~

You can pattern the implementation after `genStringFn`. Just follow the types!

One problem with this approach is reporting test case failures back to the user. In the event of a failure, all the user will see is that for some opaque function, the property failed, which isn't very enlightening. There's been work in the Haskell library [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) to be able to report back to the user and even _shrink_ down the generated functions to the simplest form that still falsifies the property. See [this talk on shrinking and showing functions](https://www.youtube.com/watch?v=CH8UQJiv9Q4).
