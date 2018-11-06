package org.mitre.synthea.distributed.world.agents

import java.time.Period
import java.util.UUID

import akka.actor.{FSM, Props}
import org.mitre.synthea.distributed.world.concepts.{ClinicianSpecialty, Education}
import org.mitre.synthea.distributed.world.geography.Address

import scala.util.Random

// keep track of seed so it can be exported later
class Clinician(val seed: Long, val specialty: ClinicianSpecialty) extends FSM[Clinician.State, Clinician.StateData] {
  import Clinician._
  import Protocol._

  val uuid: String = UUID.randomUUID().toString

  val random: Random = new Random(seed)
  var populationSeed = 0L


  startWith(Active, Uninitialized)

  when(Active) {

    case Event("SeePatient", d: Data) =>
      log.debug("Seeing patient")
      stay using d.copy(numEncounters = d.numEncounters + 1)

    case Event("LearnMedicine", d: Data) =>
      log.debug("Learning medicine")
      stay using d.copy(numEncounters = d.numEncounters + 1)

    case Event("LearnLanguage(language)", d: Data) =>
      log.debug("Learning language")
      stay using d.copy(numEncounters = d.numEncounters + 1)

  }


}

object Clinician {

  type FIRST_LANGUAGE = String
  type GENDER = String
  type EDUCATION = String
  type LOCATION = String

  case class ClinicianAddress (
                                address: String,
                                city: String,
                                state: String,
                                zip: String
                              ) extends Address

  case class ClinicianEducation(
                                 gradeSchool: Option[Boolean] = Some(true),
                                 highSchool: Option[Boolean] = Some(true),
                                 bachelors: Option[Boolean] = Some(true),
                                 masters: Option[Boolean] = Some(true)
                               ) extends Education

  // states
  sealed trait State
  case object InGradeSchool extends State
  case object InHighSchool extends State
  case object InCollege extends State
  case object InMedicalSchool extends State
  case object InResidency extends State
  case object Active extends State
  case object Retired extends State

  sealed trait StateData
  case object Uninitialized extends StateData
  case class Data(
                   random: Random,
                   name: Person.Name,
                   gender: Gender,
                   age: Period,
                   legal: Person.Legal,
                   address: ClinicianAddress,
                   education: ClinicianEducation,
                   numEncounters: Int,
                   specialty: ClinicianSpecialty,
                   servicesProvided: List[String] = List.empty[String]
                 ) extends StateData {


  def props(seed: Long, specialty: ClinicianSpecialty) = Props(new Clinician(seed, specialty))

}

