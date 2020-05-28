package io.scalac.akka.http.websockets.deleteMe

import scala.language.implicitConversions

trait Pretty[-T] {

  def pretty(x: T): String

//  def beautiful(x: T): String

}

object Pretty {

  implicit def richPretty[T: Pretty](x: T): RichPretty[T] = new RichPretty(x)

  def apply[T](prettifier: T => String /*, beautifuler: T => String*/): Pretty[T] = new Pretty[T] {
    override def pretty(x: T): String = prettifier(x)

//    override def beautiful(x: T): String = beautifuler(x)

  }

  implicit lazy val prettyString: Pretty[String] =
    Pretty(_ + "!")

  implicit lazy val prettyInt: Pretty[Int] =
    Pretty(_.toString)

  implicit lazy val prettyBoolean: Pretty[Boolean] =
    Pretty(if (_) "wahr" else "falsch")

  implicit def prettyOption[X]: Pretty[Option[Boolean]] =
    Pretty(_.fold("nichts")(_.richPretty))

  implicit def prettyStringList[T: Pretty]: Pretty[List[T]] =
    Pretty(_.map(_.richPretty).mkString(" und "))

}




object Test extends App {
  implicit def callMeHowYouLikeJustDontCallMe[T: Pretty](x: T): RichPretty[T] = new RichPretty(x)


  def printPretty[X: Pretty](x: X): Unit =
    println(x.richPretty)

  def printPretty2[X: Pretty](x: X): Unit =
    println(new RichPretty[X](x).richPretty)


  printPretty("Hallo")
  printPretty(1)
  printPretty(List("Hallo", "Moin"))
  printPretty(List(1, 2))
  printPretty(List(List("a", "b"), List("c", "d")))

  printPretty(Cow("Berta"))

}






// -----

sealed trait Animal
case class Cow(name: String) extends Animal
case class Monkey(name: String) extends Animal

object Animal {

  implicit lazy val prettyAnimal: Pretty[Animal] =
    Pretty {
      case Cow(name) => name + " mooo!"
      case Monkey(name) => name + " ugug!"
    }

}

