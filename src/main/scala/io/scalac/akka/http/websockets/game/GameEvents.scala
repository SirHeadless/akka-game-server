package io.scalac.akka.http.websockets.game

import akka.actor.ActorRef

case class GameMessage(sender: String, text: String)

object SystemMessage {
  def apply(text: String) = GameMessage("System", text)
}


sealed trait GameEvent

case class UserJoined(name: String, userActor: ActorRef) extends GameEvent

case class UserLeft(name: String) extends GameEvent

case class IncomingMessage(sender: String, message: String) extends GameEvent



