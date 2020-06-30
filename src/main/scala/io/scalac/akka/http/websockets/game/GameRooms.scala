package io.scalac.akka.http.websockets.game

import akka.actor.ActorSystem

// TODD: Make this object immutable
object GameRooms {
  var gameRooms: Map[Int, GameRoom] = Map.empty[Int, GameRoom]

  def findOrCreate(number: Int)(implicit actorSystem: ActorSystem): GameRoom = gameRooms.getOrElse(number, createNewChatRoom(number))

  private def createNewChatRoom(number: Int)(implicit actorSystem: ActorSystem): GameRoom = {
    val gameRoom = GameRoom(number)
    gameRooms += number -> gameRoom
    gameRoom
  }

}
