package io.scalac.akka.http.websockets.game

import akka.actor.{Actor, ActorLogging, ActorRef}
import io.scalac.akka.http.websockets.state.{GameState, GreenPlayer, Move, Player, StateAndChanges, BluePlayer}
import io.scalac.akka.http.websockets.terrain.{OffsetCoords, Pos}

import scala.util.{Failure, Random, Success, Try}

case object GameFinished
case object DeleteGame

class GameActor(roomId: Int, boardRows: Int, boardColumns: Int) extends Actor with ActorLogging {

  override def receive: Receive = {
    case UserLeft(name) => log.warning(s"Not existing user ${name} left the chat room ${roomId}")
    case UserJoined(name, actorRef) => context.become(Participants(Map(name -> (GreenPlayer(name), actorRef))))
    case msg: IncomingMessage => log.warning(s"Can not broadcast message ${msg} in an empty chat room ${roomId}")
  }

  def deletingGame: Receive = {
    case DeleteGame => context.stop(self)
  }

  def playerWaiting(participants: Participants): Receive = {
    case UserLeft(name) => {
      log.info(s"User $name left channel[$roomId]")
      context.become(deletingGame)
      context.self ! DeleteGame
    }
    case UserJoined(name, actorRef) =>  {
      log.info(s"User $name joined channel[$roomId]")
      broadcast(participants, s"User $name joined channel[$roomId]")
      val updatedParticipants = participants.add((name -> (BluePlayer(name), actorRef)))
      val valencesMessage = startGame.valences.flatten.foldLeft("")((a,b) =>  a + "," + b)
      println(valencesMessage)
      broadcast(updatedParticipants, "valences" + valencesMessage)
      context.become(gameStarted(updatedParticipants, Map.empty, startGame))
    }

    case msg: IncomingMessage => println(s"Game didn't start yet")
  }

  def gameStarted(participants: Participants, spectators: Map[String, ActorRef], gameState: GameState): Receive = {
    case UserLeft(name) => {
      val newParticipant = participants.remove(name)
      println(s"User $name left channel[$roomId]")
      broadcast(participants, s"User $name left channel[$roomId]", spectators)
      if (newParticipant.isEmpty) {
        context.become(receive)
      } else {
        context.become(playerWaiting(newParticipant))
      }
    }
    case UserJoined(name, actorRef) =>  {
      println(s"User $name joined channel[$roomId]")
      broadcast(participants, s"User $name joined channel[$roomId]", spectators)
      context.become(gameStarted(participants, spectators + (name -> actorRef), gameState))
    }

    case msg: IncomingMessage =>
      // TODO: Implement a better protocol to send messages (msgpack.org?)
      val stringCoords: Array[String] = msg.message.split(",")
      val player = participants(msg.sender)._1
      // TODO look scala course about TRY and see if you can improve the following commands
      val result: Try[Either[String, StateAndChanges]] =  Try((stringCoords(0).toInt, stringCoords(1).toInt))
        .map(coords =>{ println(coords); gameState.makeMove(Move(player, coords))})

      result match {
        case Success(value) => {
          value match {
            case Left(failure) =>
              // If left send the whole game state to the sender
              println(failure)
            case Right(gameStateAndChanges) =>
              println(startGame.valences)
              println(s"CAPTURED FIELDS: ${gameStateAndChanges.gameState.capturedFields.flatMap(playerToPositions => Map(playerToPositions._1 -> playerToPositions._2.map(pos => (pos.getOffsetCoords, gameState.valences(pos.getOffsetCoords.y)(pos.getOffsetCoords.x)))))}")
              participants.flatMap(x => Map(x._2._1 -> gameStateAndChanges.gameState.calculatePointsOfCapturedFields(x._2._1))).foreach(x => println("Score: " + x))
              broadcast(participants, "move," + msg.message + s",${getPlayerMessageCode(player)}", spectators)
              broadcast(participants, "captured," + getPlayerMessageCode(player) + gameStateAndChanges.stateChanges.nextCapturedFields(player).map(_.getOffsetCoords).flatMap(coords => List(coords.x, coords.y)).foldLeft("")((a, b) =>  a + "," + b))
              if (gameStateAndChanges.gameState.areAllFieldsCaptured) {
                context.become(gameFinished(participants, gameStateAndChanges.gameState))
                context.self !GameFinished
              }
              else context.become(gameStarted(participants, spectators, gameStateAndChanges.gameState))

          }
        }
        case Failure(exception) =>
          println(exception)
      }


  }

  def gameFinished(participants: Map[String, (Player, ActorRef)], finalGameState: GameState): Receive = {
    case GameFinished => {
      val playerToPoints = participants.flatMap(x => Map(x._2._1 -> finalGameState.calculatePointsOfCapturedFields(x._2._1)))
      broadcast(participants, "winner" + playerToPoints.flatMap(nameToPoints => List(getPlayerMessageCode(nameToPoints._1).toString, nameToPoints._2.toString ))
        .foldLeft("")(_+","+_) )
    }
  }

  implicit def tupleToOffsetCoords(coords: (Int,Int)): OffsetCoords = OffsetCoords(coords._1, coords._2)



  def broadcast(participants: Participants, message: GameMessage, spectators: Map[String, ActorRef] = Map.empty): Unit = {
    log.info("mmmmmmmmmmmmmmmmmmmmmmmmmmmmmm")
    log.info(message.text)
    log.info("mmmmmmmmmmmmmmmmmmmmmmmmmmmmmm")
    participants match { case ConnectedParticipants(participants) => participants.values.foreach(_._2 ! message) }
    spectators.values.foreach(_ ! message)
  }

  // Is this still functional or do I have to inject the Random.nextInt
  val values: (Int, Int) => List[List[Int]] = (rows, maxColumns) => List.tabulate(rows)(n => if (n % 2 == 0) List.fill(maxColumns)(Random.nextInt(6) + 1) else List.fill(maxColumns - 1)(Random.nextInt(6) + 1)  )

  val startGame: GameState = new GameState {
    override val moves: List[Move] = List.empty

    override def isTurn: Player => Boolean = {
      case GreenPlayer(_) => true
      case _ => false
    }
    // make implicit
    override val maxColumns: Int = 7
    override val rows: Int = 10
    override val valences: List[List[Int]] = values(maxColumns, rows)
    override val capturedFields: Map[Player, List[Pos]] = Map.empty
  }

  def getPlayerMessageCode(player: Player): Int = player match {
    case GreenPlayer(_) => 0
    case BluePlayer(_) => 1
  }
}
