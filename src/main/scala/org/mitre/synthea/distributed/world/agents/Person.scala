package org.mitre.synthea.distributed.world.agents

import java.time.{LocalDate, LocalDateTime, Period, ZoneOffset}

import akka.actor.{FSM, Props}

import scala.util.Random
import org.mitre.synthea.distributed.modules.LifecycleModule
import org.mitre.synthea.distributed.world.geography.Address


class Person(seed: Long, birthPlace: Person.BirthPlace, birthDate: Option[LocalDate]) extends FSM[Person.State, Person.StateData] {

  import Person._
  import Protocol._

  val random = new Random(seed)

  var populationSeed = 0L

  startWith(Unborn, Uninitialized)

  when(Unborn) {

    case Event(BeBorn, Uninitialized) =>
      goto(Alive) using Data(
        random = random,
        name = Name("John", "Doe", None, None, Some("Jr.")),
        gender = List("Male","Female")(random.nextInt(2)),
        age = new Period(0),
        legal = Legal(),
        socioEconomic = SocioEconomic(),
        behavior = Behavior(),
        providers = PreferredProviders.empty,
        vitalSigns = VitalSigns(),
        symptoms = Symptoms.empty,
        conditions = Conditions(),
        history = List.empty[Data],
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

    case Event(Die(cause), data: Data) =>
      goto(Dead) using data.copy(causeOfDeath = cause)

    case Event(AdvanceToDateTime(dateTime), data: Data) =>
      val state1 = LifecycleModule.process(data, dateTime.toEpochSecond(ZoneOffset.UTC))

      state1.legal.timeOfDeath match {
        case None => stay using state1
        case Some(_) => goto(Dead) using state1
      }

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
                    maritalStatus: Option[MaritalStatus] = None,
                    timeOfDeath: Option[LocalDateTime] = None
                  )

  case class BirthPlace (
                         address: String,
                         city: String,
                         state: String,
                         zip: String,
                         birthplace: String
                       ) extends Address

  case class Demographics (
                          race: RACE,
                          ethnicity: ETHNICITY,
                          language: Language
                          )

  case class Behavior(
                       smoker: Option[Boolean] = None,
                       quitSmokingProbability: Option[Double] = None,
                       quitSmokingAge: Option[Int] = None,
                       alcoholic: Option[Boolean] = None,
                       quitAlcoholismProbability: Option[Double] = None,
                       quitAlcoholismAge: Option[Int] = None,
                       adherence: Option[Boolean] = None,
                       adherenceProbability: Option[Double] = None,
                       sexualOrientation: Option[SexualOrientation] = None
                     )

  case class Conditions(
                         hypertension: Option[Boolean] = None,
                         diabetes: Option[Boolean] = None,
                         prediabetes: Option[Boolean] = None,
                         diabetesSeverity: Option[Int] = None,
                         diabeticKidneyDamage: Option[Int] = None,
                         osteoporosis: Option[Boolean] = None,
                         probabilityOfFallInjury: Option[Double] = None
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

  case class VitalSigns(
                         height: Option[Double] = None,
                         weight: Option[Double] = None,
                         heightPercentile: Option[Double] = None,
                         weightPercentile: Option[Double] = None,
                         bmi: Option[Double] = None,
                         systolicBloodPressure: Option[Double] = None,
                         diastolicBloodPressure: Option[Double] = None,
                         totalCholesterol: Option[Double] = None,
                         triglycerides: Option[Double] = None,
                         hdl: Option[Double] = None,
                         ldl: Option[Double] = None,
                         bloodGlucose: Option[Double] = None,
                         egfr: Option[Int] = None,
                         microalbuminCreatinineRatio: Option[Double] = None,
                         creatinine: Option[Double] = None,
                         ureaNitrogen: Option[Double] = None,
                         calcium: Option[Double] = None,
                         glucose: Option[Double] = None,
                         chloride: Option[Double] = None,
                         potassium: Option[Double] = None,
                         carbonDioxide: Option[Double] = None,
                         sodium: Option[Double] = None
                       )

  // states
  sealed trait State
  case object Unborn extends State
  case object Alive extends State
  case object Dead extends State

  sealed trait StateData

  case object Uninitialized extends StateData

  case class Data(random: Random,
                  name: Name,
                  gender: Gender,
                  age: Period,
                  legal: Legal,
                  socioEconomic: SocioEconomic,
                  behavior: Behavior,
                  providers: PreferredProviders,
                  vitalSigns: VitalSigns,
                  symptoms: Symptoms,
                  conditions: Conditions,
                  history: List[Data],
                  encounters: Encounters,
                  causeOfDeath: CauseOfDeath
                 ) extends StateData {

    def rand(xs: Array[Double]): Double = {
      random.nextDouble * (xs(1) - xs(0)) + xs(0)
    }

    def rand(xs: Array[Int]): Int = {
      random.nextInt * (xs(1) - xs(0)) + xs(0)
    }

  }

  def props(seed: Long, birthPlace: BirthPlace, birthDate: Option[LocalDate]) = Props(new Person(seed, birthPlace, birthDate))


}

