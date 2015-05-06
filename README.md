[![Build status](https://travis-ci.org/fpinscala/fpinscala.svg?branch=master)](https://travis-ci.org/fpinscala/fpinscala)


(This is my ([wjd](https://github.com/williamdemeo)) version of the README
file.  For the [original version](#original-readme-file) scroll down, or go to
the original fpinscala git repository at the url above.)


To get started with the exercises in the book "Functional Programming in Scala",
I followed these steps:

1. Add the eclipse plugin to my sbt install by putting the following
   line in the file ~/.sbt/0.13/plugins/plugins.sbt

        addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "3.0.0")

   (Note: I already had sbt installed.)

2. Fork the fpinscala repository from https://github.com/fpinscala/fpinscala into my own GitHub account.

3. Clone my forked version of the repository to my local hard drive:

        git clone git@github.com:williamdemeo/fpinscala.git
	
4. Enter the resulting fpinscala directory and execute the following:

        ~/git/Scala/fpinscala$ sbt
        > project exercises
        > compile
		> eclipse

   This switches to the exercises project, where your code lives, compiles the
   code, and then generates Eclipse files necessary to import the project into
   Eclipse.

5. Import project into Eclipse:

        File --> Import --> Git --> Projects From Git --> etc.

   The result: three projects are imported into Eclipse, called `answers`,
   `chapter-code`, and `exercises`.

6. To use [ScalaCheck](https://www.scalacheck.org/) and
   [ScalaTest](http://www.scalatest.org/) for testing my solutions to the
   exercises, I added the file build.sbt in the exercises directory, with the
   following lines: 

        libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

        libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

        libraryDependencies += "junit" % "junit" % "4.10" % "test"

   (These commands must be separated by blank lines, as shown above.)
   Then the `eclipse` command from step 4 above must be invoked one more time:
   
        ~/git/Scala/fpinscala$ sbt
		> eclipse


---------------------------------

## Original Readme File

*The contents of the original readme file appear below.*

This repository contains exercises, hints, and answers for the book
[Functional Programming in Scala](http://manning.com/bjarnason/). Along
with the book itself, it's the closest you'll get to having your own
private functional programming tutor without actually having one.

Here's how to use this repository:

Each chapter in the book develops a fully working library of functions
and data types, built up through a series of exercises and example code
given in the book text. The shell of this working library and exercise
stubs live in
`exercises/src/main/scala/fpinscala/<chapter-description>`, where
`<chapter-description>` is a package name that corresponds to the
chapter title (see below). When you begin working on a chapter, we
recommend you open the exercise file(s) for that chapter, and when you
encounter exercises, implement them in the exercises file and make sure
they work.

If you get stuck on an exercise, let's say exercise 4 in the chapter,
you can find hints in `answerkey/<chapter-description>/04.hint.txt` (if
no hints are available for a problem, the file will just have a single
'-' as its contents) and the answer along with an explanation of the
answer and any variations in
`answerkey/<chapter-description>/04.answer.scala` or
`04.answer.markdown`. The finished Scala modules, with all answers for
each chapter live in
`answers/src/main/scala/fpinscala/<chapter-description>`. Please feel
free to submit pull requests for alternate answers, improved hints, and
so on, so we can make this repo the very best resource for people
working through the book.

Chapter descriptions:

* Chapter 2: gettingstarted
* Chapter 3: datastructures
* Chapter 4: errorhandling
* Chapter 5: laziness
* Chapter 6: state
* Chapter 7: parallelism
* Chapter 8: testing
* Chapter 9: parsing
* Chapter 10: monoids
* Chapter 11: monads
* Chapter 12: applicative
* Chapter 13: iomonad
* Chapter 14: localeffects
* Chapter 15: streamingio

To build the code for the first time, if on windows:

    $ .\sbt.cmd

If on mac/linux, first make sure you have not checked out the code onto
an encrypted file system, otherwise you will get compile errors
regarding too long file names (one solution is to put the fpinscala repo
on a unencrypted usb key, and symlink it into your preferred code
location).

    $ chmod a+x ./sbt
    $ ./sbt

This will download and launch [sbt](http://scala-sbt.org), a build tool
for Scala. Once it is finished downloading, you'll get a prompt from
which you can issue commands to build and interact with your code. Try
the following:

    > project exercises
    > compile

This switches to the exercises project, where your code lives, and
compiles the code. You can also do:

    > console

to get a Scala REPL with access to your exercises, and

    > run

To get a menu of possible main methods to execute.

To create project files for the eclipse IDE you can install the
[sbteclipse](https://github.com/typesafehub/sbteclipse)
[sbt](http://scala-sbt.org) plugin. This makes a new command available
in [sbt](http://scala-sbt.org):

    > eclipse

All code in this repository is
[MIT-licensed](http://opensource.org/licenses/mit-license.php). See the
LICENSE file for details.

Have fun, and good luck! Also be sure to check out [the community
wiki](https://github.com/fpinscala/fpinscala/wiki) for the **chapter
notes**, links to more reading, and more.

_Paul and Rúnar_

