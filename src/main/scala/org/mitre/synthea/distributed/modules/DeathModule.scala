package org.mitre.synthea.distributed.modules

import org.mitre.synthea.distributed.world.agents.Person
import org.mitre.synthea.world.concepts.HealthRecord.Code
import org.mitre.synthea.world.concepts.HealthRecord.Encounter
import org.mitre.synthea.world.concepts.HealthRecord.Observation
import org.mitre.synthea.world.concepts.HealthRecord.Report
import java.util
import java.util.{Arrays, Collection}

import akka.actor.{Actor, ActorLogging, ActorRef}
import org.mitre.synthea.world.concepts.HealthRecord

class DeathModule(dataStore: ActorRef) extends Actor with ActorLogging {
  import DeathModule._

  override def receive: Receive = {
    case ProcessDeath(p, t) => process(p, t)
  }

  def process(person: Person, time: Long): Unit = {
    if (!person.alive(time) && person.causeOfDeath.isDefined) { // create an encounter, diagnostic report, and observation
      val causeOfDeath = person.causeOfDeath.get

      val deathCertification = person.record.encounterStart(time, "ambulatory")
      deathCertification.codes.add(DEATH_CERTIFICATION)
      val codObs = person.record.observation(time, CAUSE_OF_DEATH_CODE.code, causeOfDeath)
      codObs.codes.add(CAUSE_OF_DEATH_CODE)
      codObs.category = "exam"
      val deathCert = person.record.report(time, DEATH_CERTIFICATE.code, 1)
      deathCert.codes.add(DEATH_CERTIFICATE)

      dataStore ! "a person died"
    }
  }

}

object DeathModule {

  case class ProcessDeath(person: Person, time: Long)

  val DEATH_CERTIFICATION = new HealthRecord.Code("SNOMED-CT", "308646001", "Death Certification")
  val CAUSE_OF_DEATH_CODE = new HealthRecord.Code("LOINC", "69453-9", "Cause of Death [US Standard Certificate of Death]")
  val DEATH_CERTIFICATE = new HealthRecord.Code("LOINC", "69409-1", "U.S. standard certificate of death - 2003 revision")

  // NOTE: if new codes are added, be sure to update getAllCodes below

  /**
    * Get all of the Codes this module uses, for inventory purposes.
    *
    * @return Collection of all codes and concepts this module uses
    */
  lazy val getAllCodes: List[HealthRecord.Code] = List(DEATH_CERTIFICATION, CAUSE_OF_DEATH_CODE, DEATH_CERTIFICATE)
}
