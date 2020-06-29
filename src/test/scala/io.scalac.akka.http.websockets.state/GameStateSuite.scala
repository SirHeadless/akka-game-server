package io.scalac.akka.http.websockets.state

import io.scalac.akka.http.websockets.terrain.{CubeCoords, OffsetCoords, Pos}
import org.scalatest.FunSuite

import scala.util.Random
import scala.util.parsing.input.OffsetPosition



class GameStateSuite extends FunSuite {
  val randomValences: (Int, Int) => List[List[Int]] = (rows, maxColumns) => List.tabulate(rows)(n => if (n % 2 == 0) List.fill(maxColumns)(Random.nextInt(6) + 1) else List.fill(maxColumns - 1)(Random.nextInt(6) + 1)  )


  val startGame: GameState = new GameState {
    override val moves: List[Move] = List.empty

    override def isTurn: Player => Boolean = {
      case GreenPlayer(_) => true
      case _ => false
    }
    // make implicit
    override val maxColumns: Int = 7
    override val rows: Int = 10
    override val valences: List[List[Int]] = randomValences(maxColumns, rows)
    override val capturedFields: Map[Player, List[Pos]] = Map.empty
  }

  test("Test capturedFields in the game state") {
    val yellowPlayer = YellowPlayer("player1")
    val greenPlayer = GreenPlayer("player2")
    val gameState = for {
      gameStateAndChanges1 <- startGame.makeMove(Move(greenPlayer, OffsetCoords(0,0)))
      gameStateAndChanges2 <- gameStateAndChanges1.gameState.makeMove(Move(yellowPlayer, OffsetCoords(3,3)))
      gameStateAndChanges3 <- gameStateAndChanges2.gameState.makeMove(Move(greenPlayer, OffsetCoords(1,0)))
    } yield gameStateAndChanges3
    gameState.map(_.gameState).map(state => state.capturedFields(greenPlayer)).foreach(xs =>
      assert(xs == List(OffsetCoords(0,0)))
    )
    gameState.map(_.gameState).map(state => state.capturedFields(yellowPlayer)).foreach(xs =>
      assert(xs == Nil)
    )
  }

  test("Test capturedFields in the game state 2") {
    val yellowPlayer = YellowPlayer("player1")
    val greenPlayer = GreenPlayer("player2")
    val gameState = for {
      gameStateAndChanges1 <- startGame.makeMove(Move(greenPlayer, OffsetCoords(1,1)))
      gameStateAndChanges2 <- gameStateAndChanges1.gameState.makeMove(Move(yellowPlayer, OffsetCoords(0,5)))
      gameStateAndChanges3 <- gameStateAndChanges2.gameState.makeMove(Move(greenPlayer, OffsetCoords(2,1)))
      gameStateAndChanges4 <- gameStateAndChanges3.gameState.makeMove(Move(yellowPlayer, OffsetCoords(1,5)))
      gameStateAndChanges5 <- gameStateAndChanges4.gameState.makeMove(Move(greenPlayer, OffsetCoords(2,2)))
      gameStateAndChanges6 <- gameStateAndChanges5.gameState.makeMove(Move(yellowPlayer, OffsetCoords(2,5)))
      gameStateAndChanges7 <- gameStateAndChanges6.gameState.makeMove(Move(greenPlayer, OffsetCoords(1,2)))
    } yield gameStateAndChanges7
    gameState.map(_.gameState).map(state => state.capturedFields(greenPlayer)).map(_.foreach(x => println(x.getOffsetCoords)))
    gameState.map(_.gameState).map(state => state.capturedFields(greenPlayer)).foreach(xs =>
      assert(xs == List(CubeCoords(1,-3,2), CubeCoords(1,-2,1)))
    )
    gameState.map(_.gameState).map(state => state.capturedFields(yellowPlayer)).foreach(xs =>
      assert(xs == Nil)
    )
  }

  test("Test capturedFields in the game state 3") {
    val yellowPlayer = YellowPlayer("player1")
    val greenPlayer = GreenPlayer("player2")
    val gameState = for {
      gameStateAndChanges1 <- startGame.makeMove(Move(greenPlayer, OffsetCoords(4,2)))
      gameStateAndChanges2 <- gameStateAndChanges1.gameState.makeMove(Move(yellowPlayer, OffsetCoords(3,4)))
      gameStateAndChanges3 <- gameStateAndChanges2.gameState.makeMove(Move(greenPlayer, OffsetCoords(5,2)))
      gameStateAndChanges4 <- gameStateAndChanges3.gameState.makeMove(Move(yellowPlayer, OffsetCoords(2,5)))
      gameStateAndChanges5 <- gameStateAndChanges4.gameState.makeMove(Move(greenPlayer, OffsetCoords(3,3)))
      gameStateAndChanges6 <- gameStateAndChanges5.gameState.makeMove(Move(yellowPlayer, OffsetCoords(3,5)))
      gameStateAndChanges7 <- gameStateAndChanges6.gameState.makeMove(Move(greenPlayer, OffsetCoords(4,3)))
    } yield gameStateAndChanges7
    println("Points:" + gameState.map(_.gameState).map(_.calculatePointsOfCapturedFields(greenPlayer)))
    gameState.map(_.gameState).map(state => state.capturedFields(greenPlayer)).map(_.foreach(x => println(x.getOffsetCoords)))
    gameState.map(_.gameState).map(state => state.capturedFields(greenPlayer)).foreach(xs =>
      assert(xs == List(OffsetCoords(4,3), OffsetCoords(4,2)))
    )
    gameState.map(_.gameState).map(state => state.capturedFields(yellowPlayer)).foreach(xs =>
      assert(xs == Nil)
    )
  }

  test("Test calculatePointsOfCapturedFields") {
    val yellowPlayer = YellowPlayer("player1")
    val greenPlayer = GreenPlayer("player2")
    val gameState = for {
      gameStateAndChanges1 <- startGame.makeMove(Move(greenPlayer, OffsetCoords(4,2)))
      gameStateAndChanges2 <- gameStateAndChanges1.gameState.makeMove(Move(yellowPlayer, OffsetCoords(3,4)))
      gameStateAndChanges3 <- gameStateAndChanges2.gameState.makeMove(Move(greenPlayer, OffsetCoords(5,2)))
      gameStateAndChanges4 <- gameStateAndChanges3.gameState.makeMove(Move(yellowPlayer, OffsetCoords(2,5)))
      gameStateAndChanges5 <- gameStateAndChanges4.gameState.makeMove(Move(greenPlayer, OffsetCoords(3,3)))
      gameStateAndChanges6 <- gameStateAndChanges5.gameState.makeMove(Move(yellowPlayer, OffsetCoords(3,5)))
      gameStateAndChanges7 <- gameStateAndChanges6.gameState.makeMove(Move(greenPlayer, OffsetCoords(4,3)))
    } yield gameStateAndChanges7
    println("Points:" + gameState.map(_.gameState).map(_.calculatePointsOfCapturedFields(greenPlayer)))
    gameState.map(_.gameState).map(state => state.capturedFields(greenPlayer)).map(_.foreach(x => println(x.getOffsetCoords)))
    gameState.map(_.gameState).map(state => state.calculatePointsOfCapturedFields(greenPlayer)).foreach((xs: Int) =>
      assert(xs > 0)
    )
  }

  test("Test calculatePointsOfCapturedFields2 ") {
    val greenPlayer = GreenPlayer("player1")
    val yellowPlayer = YellowPlayer("player2")
    val gameStateRow0 = setFieldsOfRow(0, 10, List(greenPlayer, yellowPlayer), startGame)
    val gameStateRow1 = setFieldsOfRow(1,9, List(greenPlayer, yellowPlayer), gameStateRow0)
    val gameStateRow2 = setFieldsOfRow(2,10, List(yellowPlayer, greenPlayer), gameStateRow1)
    val gameStateRow3 = setFieldsOfRow(3, 9, List(yellowPlayer, greenPlayer), gameStateRow2)
    val gameStateRow4 = setFieldsOfRow(4,10, List(greenPlayer, yellowPlayer), gameStateRow3)
    val gameStateRow5 = setFieldsOfRow(5,9, List(greenPlayer, yellowPlayer), gameStateRow4)
    val gameStateRow6 = setFieldsOfRow(6, 10, List(yellowPlayer, greenPlayer), gameStateRow5)

    println(gameStateRow6.calculatePointsOfCapturedFields(greenPlayer))
    println(gameStateRow6.calculatePointsOfCapturedFields(yellowPlayer))
  }

  test("Valences do not change after move ") {
    val greenPlayer = GreenPlayer("player1")
    val yellowPlayer = YellowPlayer("player2")
    val gameStateRow0 = setFieldsOfRow(0, 10, List(greenPlayer, yellowPlayer), startGame)

    assert(startGame.valences == gameStateRow0.valences)


  }

  def setFieldsOfRow(row: Int, numberOfFields: Int, players: List[Player], gameState: GameState): GameState = {
    println(row)
    (0 until numberOfFields).foldLeft(gameState)(
      (gameState , i ) =>
        gameState.makeMove(Move(players(i % 2), OffsetCoords(i, row))) match { case Right(stateAndChanges) => stateAndChanges.gameState }
    )
  }

}
