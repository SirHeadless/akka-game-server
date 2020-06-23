package io.scalac.akka.http.websockets.chat

import akka.actor.{Actor, ActorLogging, ActorRef}
import io.scalac.akka.http.websockets.state.{GameState, GreenPlayer, Move, Player, YellowPlayer}
import io.scalac.akka.http.websockets.terrain.{OffsetCoords, Pos}

import scala.util.{Failure, Random, Success, Try}

class GameActor(roomId: Int, boardRows: Int, boardColumns: Int) extends Actor with ActorLogging {

  override def receive: Receive = {
    case UserLeft(name) => log.warning(s"Not existing user ${name} left the chat room ${roomId}")
    case UserJoined(name, actorRef) => context.become(playerWaitingChatRoom(Map(name -> (GreenPlayer(name), actorRef))))
    case msg: IncomingMessage => log.warning(s"Can not broadcast message ${msg} in an empty chat room ${roomId}")
  }

  def playerWaitingChatRoom(participants: Map[String, (Player, ActorRef)]): Receive = {
    case UserLeft(name) => {
      println(s"User $name left channel[$roomId]")
      context.become(receive)
    }
    case UserJoined(name, actorRef) =>  {
      println(s"User $name joined channel[$roomId]")
      broadcast(participants, s"User $name joined channel[$roomId]")
      val updatedParticipants = participants + (name -> (YellowPlayer(name), actorRef))
      broadcast(updatedParticipants, startGame.valences.toString())
      context.become(filedChatRoom(updatedParticipants, Map.empty, startGame))
    }

//    case msg: IncomingMessage => broadcast(participants, msg)
    case msg: IncomingMessage => println(s"Game didn't start yet")
  }

//  def gameStartedChatRoom(participants: Map[String, ActorRef]): Receive = {
//    case UserLeft(name) => {
//      println(s"User $name left channel[$roomId]")
//      broadcast(participants, "Player 1 Won Match")
//      context.become(receive)
//    }
//    case UserJoined(name, actorRef) =>  {
//      println(s"User $name joined channel[$roomId]")
//      broadcast(participants, s"User $name joined channel[$roomId]")
//      context.become(gameStartedChatRoom(participants + (name -> actorRef)))
//    }
//
//    //    case msg: IncomingMessage => broadcast(participants, msg)
//    case msg: IncomingMessage => println(s"Game didn't start yet")
//  }

  implicit def tupleToOffsetCoords(coords: (Int,Int)): OffsetCoords = OffsetCoords(coords._1, coords._2)

  def filedChatRoom(participants: Map[String, (Player, ActorRef)], spectators: Map[String, ActorRef], gameState: GameState): Receive = {
    case UserLeft(name) if participants.contains(name) => {
      val newParticipant = participants - name
      println(s"User $name left channel[$roomId]")
      broadcast(participants, s"User $name left channel[$roomId]", spectators)
      if (newParticipant.isEmpty) {
        context.become(receive)
      } else {
        context.become(playerWaitingChatRoom(newParticipant))
      }
    }
    case UserJoined(name, actorRef) =>  {
      println(s"User $name joined channel[$roomId]")
      broadcast(participants, s"User $name joined channel[$roomId]", spectators)
      context.become(filedChatRoom(participants, spectators + (name -> actorRef), gameState))
    }

    case msg: IncomingMessage =>
      // TODO: Implement a better protocol to send messages (msgpack.org?)
      val stringCoords: Array[String] = msg.message.split(",")
      val player = participants(msg.sender)._1
      // TODO look scala course about TRY and see if you can improve the following commands
      val result: Try[Either[String, GameState]] =  Try((stringCoords(0).toInt, stringCoords(1).toInt))
        .map(coords =>{ println(coords); gameState.makeMove(Move(player, coords))})

      result match {
        case Success(value) => {
          value match {
            case Left(failure) =>
              // If left send the whole game state to the sender
              println(failure)
            case Right(gameState) =>
              broadcast(participants, msg.message + s",${getPlayerMessageCode(player)}", spectators)
              context.become(filedChatRoom(participants, spectators, gameState))
          }
        }
        case Failure(exception) =>
          println(exception)
      }


  }

  def broadcast(participants: Map[String, (Player, ActorRef)],  message: ChatMessage, spectators: Map[String, ActorRef] = Map.empty): Unit = {
    participants.values.foreach(_._2 ! message)
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
    override val valences: List[List[Int]] = values(rows, maxColumns)
    override val capturedFieldsYellow: List[Pos] = List.empty
    override val capturedFieldsGreen: List[Pos] = List.empty
  }

  def getPlayerMessageCode(player: Player): Int = player match {
    case GreenPlayer(_) => 0
    case YellowPlayer(_) => 1
  }
}
