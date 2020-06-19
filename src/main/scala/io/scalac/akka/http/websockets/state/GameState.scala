package io.scalac.akka.http.websockets.state

import io.scalac.akka.http.websockets.terrain.{Pos, Terrain}

import scala.util.Random

trait GameSymbol {
  val pos: Pos
}

case class GreenSymbol(pos: Pos) extends GameSymbol
case class YellowSymbol(pos: Pos) extends GameSymbol
case class NoSymbol(pos: Pos) extends GameSymbol
case class NotOnBoard(pos: Pos) extends GameSymbol

trait Player {
  val name: String
}

case class GreenPlayer(name: String) extends Player
case class YellowPlayer(name: String) extends Player

case class Move(player: Player, pos: Pos)

trait GameState(values: List[List[Int]]) extends Terrain {
  self =>

  val moves: List[Move]

  def isTurn: Player => Boolean

  def makeMove(move: Move): Either[String, GameState] = move match {
    case Move(player, pos) =>
      if (!isTurn(player)) Left(s"It is not the turn of ${player}")
      else if (!terrainFunction(pos)) Left(s"Pos ${pos} is not on the board")
      else {
        if (moves.map(_.pos).contains(pos)) Left(s"Move ${move} not possible. Field already filled")
        else Right(new GameState {
          override val moves: List[Move] = move:: self.moves
          override val rows: Int = self.rows
          override val maxColumns: Int = self.maxColumns
          override def isTurn: Player => Boolean = player => !self.isTurn(player)
        })
      }
  }



}
