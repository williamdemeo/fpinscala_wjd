/* File: Parsers.scala (Ch 9)
 * Authors: Paul Chiusano and Runar Bjarnason
 * Url: https://github.com/fpinscala/fpinscala 
 * 
 * Description: This is a modified version of the file Parsers.scala
 *   that accompanies the book "Functional Programming in Scala" by
 *   Chiusano and Bjarnason. This version of the file includes 
 *   solutions to some of the exercises in 
 * 
 *     CHAPTER 9: Parser Combinators
 * 
 *   The solutions herein are by William DeMeo <williamdemeo@gmail.com>.
 *   They are at best imperfect, and possibly wrong.  Official solutions by 
 *   Chiusano and Bjarnason are available in the github repo mentioned above.
 */
package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import scala.language.higherKinds
import scala.language.implicitConversions
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

	def run[A](p: Parser[A])(input: String): Either[ParseError, A]

	implicit def string(s: String): Parser[String]
	implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
	implicit def asStringParser[A](a:A)(implicit f: A => Parser[String]):
		ParserOps[String] = ParserOps(f(a))
	
	def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

	def defaultSucceed[A](a:A): Parser[A] = string("") map (_ => a)

  def succeed[A](a:A): Parser[A]

	def slice[A](p: Parser[A]): Parser[String]
	def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_::_)
	
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B] 
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_::_) or succeed(List())
	
	def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

	implicit def regex(r: Regex): Parser[String]

	def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = 
		flatMap(p)(a => map(p2)(b => (a,b)))
	
	def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = 
		p flatMap( a => p2 map( b => f(a,b)))

	def map[A,B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(f andThen succeed)

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  /** Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_,b) => b)

  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result. */
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a,b) => a)

  def opt[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) or succeed(None)

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = "\\s*".r

  /** Parser which consumes 1 or more digits. */
  def digits: Parser[String] = "\\d+".r

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = (".*?"+Pattern.quote(s)).r

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
    // rather annoying to write, left as an exercise
    // we'll just use quoted (unescaped literals) for now
    token(quoted label "string literal")

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
    * Result is left as a string to keep full precision
    */
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p,p2) or succeed(List())

  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  /** Parses a sequence of left-associative binary operators with the same precedence. */
  def opL[A](p: Parser[A])(op: Parser[(A,A) => A]): Parser[A] =
    map2(p, many(op ** p))((h,t) => t.foldLeft(h)((a,b) => b._1(a,b._2)))

  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof
	
  case class ParserOps[A](p: Parser[A]) {
  	def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  	def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

  	def map[B](f: A => B): Parser[B] = self.map(p)(f)
  	def many = self.many(p)

  	def slice: Parser[String] = self.slice(p)
  	
  	def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
		def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)

  	def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  	
  	def label(msg: String): Parser[A] = self.label(msg)(p)
  	def scope(msg: String): Parser[A] = self.label(msg)(p)
 		def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*(p2: => Parser[Any]) = self.skipR(p, p2)
    def token = self.token(p)
    def sep(separator: Parser[Any]) = self.sep(p, separator)
    def sep1(separator: Parser[Any]) = self.sep1(p, separator)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
    def opL(op: Parser[(A,A) => A]): Parser[A] = self.opL(p)(op)
  	//def map2[A,B,C](p: Parser[A], p2: Parser[A])(f: (A,B) => C): Parser[C]
  }

  object Laws {
  	def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = 
  		forAll(in)(s => run(p1)(s) == run(p2)(s))
  	
  	def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
  		
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}