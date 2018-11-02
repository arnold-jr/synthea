package org.mitre.synthea.distributed.world.agents

import java.time.{LocalDate, Period}

import akka.actor.{FSM, Props}

import scala.util.Random
import Person._
import org.mitre.synthea.world.agents.Person
import org.mitre.synthea.world.concepts.VitalSign


class Person(seed: Long, birthPlace: BirthPlace, birthDate: Option[LocalDate]) extends FSM[State, Data] {

  import Protocol._

  val random = new Random(seed)
  var populationSeed = 0L

  startWith(Unborn, Uninitialized)

  when(Unborn) {

    case Event(BeBorn, Uninitialized) =>
      goto(Alive) using StateData(
        random = random,
        name = Name("John", "Doe", None, None, Some("Jr.")),
        gender = List("Male","Female")(random.nextInt(2)),
        age = new Period(0),
        legal = Legal(),
        socioEconomic = SocioEconomic(),
        providers = PreferredProviders.empty,
        vitalSigns = VitalSigns(),
        symptoms = Symptoms.empty,
        history = List.empty[StateData],
        encounters = Encounters.empty,
        causeOfDeath = None
      )

    case Event(BeBorn, _) =>
      log.error("Whoops, I was already initialized but in the Unborn state")
      stay

  }

  onTransition{
    case Unborn -> Alive =>
      log.info("Notify the birth registry that I was born")
    case Alive -> Dead =>
      log.info("Notify the death registry that I died")
  }

  when(Alive) {

    case Event(Die(cause), data: StateData) =>
      goto(Dead) using data.copy(causeOfDeath = cause)

    case Event(AdvanceDays(days), data: StateData) =>
      stay using data.copy()
  }

  whenUnhandled {
    case Event(BeBorn, _) =>
      log.error("Whoops, I can only be born once")
      stay
  }

  initialize()



}

object Person {

  case class Name(
                   firstName: String,
                   lastName: String,
                   maidenName: Option[String],
                   namePrefix: Option[String],
                   nameSuffix: Option[String]
                 ) {
    val name = s"$firstName $lastName"
  }

  case class Legal(
                    ssn: Option[Int] = None,
                    driversId: Option[String] = None,
                    passportId: Option[String] = None,
                    maritalStatus: Option[MaritalStatus] = None
                  )

  case class BirthPlace(
                         address: String,
                         city: String,
                         state: String,
                         zip: String,
                         birthplace: String
                       )

  case class Behavior(
                       smoker: Some[Boolean],
                       alcoholic: Some[Boolean],
                       adherence: Some[Float],
                       sexualOrientation: Some[SexualOrientation]
                     )

  case class SocioEconomic(
                            firstLanguage: Option[Language] = None,
                            score: Option[SOCIOECONOMIC_SCORE] = None,
                            category: Option[SOCIOECONOMIC_CATEGORY] = None,
                            income: Option[INCOME] = None,
                            incomeLevel: Option[INCOME_LEVEL] = None,
                            education: Option[EDUCATION] = None,
                            educationLevel: Option[EducationLevel] = None,
                            occupationLevel: Option[OCCUPATION_LEVEL] = None
                          )

  // states
  sealed trait State
  case object Unborn extends State
  case object Alive extends State
  case object Dead extends State

  sealed trait Data
  case object Uninitialized extends Data
  case class StateData(random: Random,
                       name: Name,
                       gender: Gender,
                       age: Period,
                       legal: Legal,
                       socioEconomic: SocioEconomic,
                       providers: PreferredProviders,
                       vitalSigns: VitalSigns,
                       symptoms: Symptoms,
                       history: List[StateData],
                       encounters: Encounters,
                       causeOfDeath: CauseOfDeath
                      ) extends Data

  def props(seed: Long, birthPlace: BirthPlace, birthDate: Option[LocalDate]) = Props(new Person(seed, birthPlace, birthDate))


}

