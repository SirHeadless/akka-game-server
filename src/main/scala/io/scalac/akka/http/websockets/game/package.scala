package io.scalac.akka.http.websockets

import scala.language.implicitConversions

package object game {
  implicit def chatEventToChatMessage(event: IncomingMessage): GameMessage = GameMessage(event.sender, event.message)
  implicit def StringToSystemMessage(msgString: String): GameMessage =  SystemMessage(msgString)
}
