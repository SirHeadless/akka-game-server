package io.scalac.akka.http.websockets.game

import akka.actor.ActorRef
import io.scalac.akka.http.websockets.state.Player

case class ParticipantInformation(UniqueName: String, player: Player, actorRef: ActorRef)



sealed trait Participants {

  def add(newParticipant: ParticipantInformation) = this match {
    case NoParticipant => OneParticipant(newParticipant)
    case OneParticipant(participants) =>
      if (participants.UniqueName != newParticipant.UniqueName)
        TwoParticipants(participants, newParticipant)
      else this
  }

  def remove(name: String) = this match {
    case NoParticipant => this
    case OneParticipant(participants) => if (participants.UniqueName == name) NoParticipant else this
    case TwoParticipants(participant1, paricipant2) =>
      if (participant1.UniqueName == name) OneParticipant(participant1)
      else if (paricipant2.UniqueName == name) OneParticipant(paricipant2)
      else this
  }

  def getParticipantInformation(name: String): Option[ParticipantInformation]
  val getParticipantInformation: List[ParticipantInformation]

}

case object NoParticipant extends Participants {
  override def getParticipantInformation(name: String): Option[ParticipantInformation] = None

  override lazy val getParticipantInformation: List[ParticipantInformation] = Nil
}
case class OneParticipant(player1Info: ParticipantInformation) extends Participants {
  override def getParticipantInformation(name: String): Option[ParticipantInformation] =
    if (player1Info.UniqueName == name) Some(player1Info)
    else None

  override lazy val getParticipantInformation: List[ParticipantInformation] = List(player1Info)
}
case class TwoParticipants(player1Info: ParticipantInformation, player2Info: ParticipantInformation) extends Participants {
  val lazy getParticipantInformation: List[ParticipantInformation] = List(player1Info, player2Info)
  def getParticipantInformation(name: String): Option[ParticipantInformation] =
    if (player1Info.UniqueName == name) Some(player1Info)
    else if (player2Info.UniqueName == name) Some(player2Info)
    else None
}

object Participants {
  def apply(participantInformation: ParticipantInformation): Participants = OneParticipant(participantInformation)
}


