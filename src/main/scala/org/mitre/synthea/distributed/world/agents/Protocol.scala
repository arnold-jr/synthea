package org.mitre.synthea.distributed.world.agents

object Protocol {

  final case object BeBorn
  final case class Die(causeOfDeath: CauseOfDeath)

  final case class AdvanceDays(days: Int)

}
