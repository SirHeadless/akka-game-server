package io.scalac.akka.http.websockets.chat

import akka.actor.{Actor, ActorLogging, ActorRef}

class ChatRoomActor(roomId: Int) extends Actor with ActorLogging {

  override def receive: Receive = {
    case UserLeft(name) => log.warning(s"Not existing user ${name} left the chat room ${roomId}")
    case UserJoined(name, actorRef) => context.become(filedChatRoom(Map(name -> actorRef)))
    case msg: IncomingMessage => log.warning(s"Can not broadcast message ${msg} in an empty chat room ${roomId}")
  }

  def playerWaitingChatRoom(participants: Map[String, ActorRef]): Receive = {
    case UserLeft(name) => {
      println(s"User $name left channel[$roomId]")
      context.become(receive)
    }
    case UserJoined(name, actorRef) =>  {
      println(s"User $name joined channel[$roomId]")
      broadcast(participants, s"User $name joined channel[$roomId]")
      context.become(filedChatRoom(participants + (name -> actorRef)))
    }

//    case msg: IncomingMessage => broadcast(participants, msg)
    case msg: IncomingMessage => println(s"Game didn't start yet")
  }

  def gameStartedChatRoom(participants: Map[String, ActorRef]): Receive = {
    case UserLeft(name) => {
      println(s"User $name left channel[$roomId]")
      broadcast(participants, "Player 1 Won Match")
      context.become(receive)
    }
    case UserJoined(name, actorRef) =>  {
      println(s"User $name joined channel[$roomId]")
      broadcast(participants, s"User $name joined channel[$roomId]")
      context.become(gameStartedChatRoom(participants + (name -> actorRef)))
    }

    //    case msg: IncomingMessage => broadcast(participants, msg)
    case msg: IncomingMessage => println(s"Game didn't start yet")
  }

  def filedChatRoom(participants: Map[String, ActorRef]): Receive = {
    case UserLeft(name) => {
      val newParticipant = participants - name
      println(s"User $name left channel[$roomId]")
      broadcast(participants, s"User $name left channel[$roomId]")
      if (newParticipant.isEmpty) {
        context.become(receive)
      } else {
        context.become(filedChatRoom(newParticipant))
      }
    }
    case UserJoined(name, actorRef) =>  {
      println(s"User $name joined channel[$roomId]")
      broadcast(participants, s"User $name joined channel[$roomId]")
      context.become(filedChatRoom(participants + (name -> actorRef)))
    }

    case msg: IncomingMessage => broadcast(participants, msg)
  }

  def broadcast(participants: Map[String, ActorRef], message: ChatMessage): Unit = participants.values.foreach(_ ! message)
}
