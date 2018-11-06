package org.mitre.synthea.distributed.world.agents
import java.time.LocalDateTime

object Protocol {

  final case object BeBorn
  final case class Die(causeOfDeath: CauseOfDeath)

  final case class AdvanceToDateTime(dateTime: LocalDateTime)

}
