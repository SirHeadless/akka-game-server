package io.scalac.akka.http.websockets.game

import akka.actor.ActorRef
import io.scalac.akka.http.websockets.state.Player

case class ParticipantInformation(UniqueName: String, player: Player, actorRef: ActorRef)

sealed trait Participants {

  def add(newParticipants: (String, (Player, ActorRef))) = add(Map(newParticipants))

  def add(newParticipants: Map[String, (Player, ActorRef)]) = this match {
    case NoParticipants => Participants(newParticipants)
    case ConnectedParticipants(participants) => Participants(participants ++ newParticipants)
  }

  def remove(name: String) = this match {
    case NoParticipants => this
    case ConnectedParticipants(participants) => {
      Participants(participants - name)
    }
  }

}

case object NoParticipant extends Participants
case class OneParticipant(participantInformation: ParticipantInformation) extends Participants
case class TwoParticipants(player1Info: ParticipantInformation, player2Info: ParticipantInformation) extends Participants

object Participants {
  def apply(participants: Map[String, (Player, ActorRef)]): Participants = participants match {
    case Map.empty => NoParticipants
    case participants => ConnectedParticipants(participants)
  }
}


