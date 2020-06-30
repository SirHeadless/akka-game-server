package io.scalac.akka.http.websockets.state

import io.scalac.akka.http.websockets.state.GameState.CapturedFields
import io.scalac.akka.http.websockets.terrain.{CubeCoords, OffsetCoords, Pos, Terrain}

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

case class BluePlayer(name: String) extends Player

case class Move(player: Player, pos: Pos)


case class StateAndChanges(gameState: GameState, stateChanges: StateChanges)

case class StateChanges(nextCapturedFields: CapturedFields)

trait GameState extends Terrain {
  self =>


  val valences: List[List[Int]]
  val moves: List[Move]

  def isTurn: Player => Boolean


  val capturedFields: Map[Player, List[Pos]]

  def makeMove(move: Move): Either[String, StateAndChanges] = move match {
    case Move(player, pos) =>
      println(s"Player ${player} selected field ${pos}")
      if (!isTurn(player)) Left(s"It is not the turn of ${player}")
      else if (!terrainFunction(pos)) Left(s"Pos ${pos} is not on the board")
      else {
        if (moves.map(_.pos).contains(pos)) Left(s"Move ${move} not possible. Field already filled")
        else {

          val nextCapturedFields: CapturedFields = capturedFields + (player -> (newCapturedFields(pos, move) ::: capturedFields.getOrElse(player, Nil)))
//          val newCapturedFields: CapturedFields = if (!isFieldAlreadyCaptured(pos) && isFieldCapturedConditionSatisfied(pos, player))
//            capturedFields + (player -> (pos :: capturedFields(player)))
//          else
//            capturedFields

          val newGameState = new GameState {
            override val capturedFields: CapturedFields = nextCapturedFields
            override val moves: List[Move] = move :: self.moves
            override val rows: Int = self.rows
            override val maxColumns: Int = self.maxColumns

            override def isTurn: Player => Boolean = player => !self.isTurn(player)

            override val valences: List[List[Int]] = self.valences
          }

          Right(StateAndChanges(newGameState, StateChanges(nextCapturedFields)))
        }
      }
  }

  def newCapturedFields(pos: Pos, move: Move): List[Pos] =
    (pos :: getNeighbours(pos)).filter(p => !isFieldAlreadyCaptured(p) && isFieldCapturedConditionSatisfied(p, move))

  def isBoardFull: Boolean = moves.size == (rows * maxColumns) - maxColumns/2

  def areAllFieldsCaptured: Boolean = capturedFields.flatMap(_._2).size == (rows * maxColumns) - maxColumns/2

  def isFieldAlreadyCaptured(pos: Pos): Boolean = {
    capturedFields.values.foldLeft(false)((acc, poss) => acc || poss.contains(pos))
  }

  def isFieldCapturedConditionSatisfied(pos: Pos, move: Move): Boolean = {
    val fieldWithNeighbours = pos :: getNeighbours(pos)
    val playersSelectedNeighbours = fieldWithNeighbours.filter(field => (move :: moves).contains(Move(move.player, field)))
    playersSelectedNeighbours.size >= fieldWithNeighbours.size.toFloat / 2
  }

  implicit def offsetCoordsToTuple(pos: Pos): (Int, Int) = pos match {
    case OffsetCoords(x,y) => (x,y)
    case cube @ CubeCoords(_,_,_) => offsetCoordsToTuple(cube.getOffsetCoords)
  }


  def calculatePointsOfCapturedFields(player: Player): Int = {
    println(capturedFields.getOrElse(player, Nil).map(pos => valences(pos.getOffsetCoords.y)(pos.getOffsetCoords.x)))
    capturedFields.getOrElse(player, Nil).map(pos => valences(pos.getOffsetCoords.y)(pos.getOffsetCoords.x)).sum
  }
}

object GameState {

  type CapturedFields = Map[Player, List[Pos]]


}

