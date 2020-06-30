package io.scalac.akka.http.websockets.game

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.stream.{FlowShape, OverflowStrategy}
import akka.stream.scaladsl.GraphDSL.Implicits._
import akka.stream.scaladsl.{Source, _}

class GameRoom(roomId: Int, actorSystem: ActorSystem) {

  private[this] val gameActor = actorSystem.actorOf(Props(classOf[GameActor], roomId, 7, 10))


  def websocketFlow(user: String): Flow[Message, Message, _] =
    Flow.fromGraph(GraphDSL.create(Source.actorRef[GameMessage](bufferSize = 5, OverflowStrategy.fail))  {
      implicit builder =>
        chatSource =>
          //flow used as input it takes Message's
          val fromWebsocket = builder.add(
            Flow[Message].collect {
              case TextMessage.Strict(txt) => IncomingMessage(user, txt)
            })

          //flow used as output, it returns Message's
          val backToWebsocket = builder.add(
            Flow[GameMessage].map {
//              case ChatMessage(author, text) => TextMessage(s"[$author]: $text")
              case GameMessage(author, text) => TextMessage(text)
            }
          )

          //send messages to the actor, if send also UserLeft(user) before stream completes.
          val chatActorSink = Sink.actorRef[GameEvent](gameActor, UserLeft(user))

          //merges both pipes
          val merge = builder.add(Merge[GameEvent](2))

          //Materialized value of Actor who sit in chatroom
          val actorAsSource = builder.materializedValue.map(actor => UserJoined(user, actor))

          //Message from websocket is converted into IncommingMessage and should be send to each in room
          fromWebsocket ~> merge.in(0)

          //If Source actor is just created should be send as UserJoined and registered as particiant in room
          actorAsSource ~> merge.in(1)

          //Merges both pipes above and forward messages to chatroom Represented by gameActor
          merge ~> chatActorSink

          //Actor already sit in chatRoom so each message from room is used as source and pushed back into websocket
          chatSource ~> backToWebsocket

          FlowShape(fromWebsocket.in , backToWebsocket.out)
    })

  def sendMessage(message: GameMessage): Unit = gameActor ! message

}

object GameRoom {
  def apply(roomId: Int)(implicit actorSystem: ActorSystem) = new GameRoom(roomId, actorSystem)
}