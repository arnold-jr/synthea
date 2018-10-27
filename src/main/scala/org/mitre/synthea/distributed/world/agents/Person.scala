package org.mitre.synthea.distributed.world.agents

import java.io.Serializable
import java.time.{Instant, Period, ZoneId}
import java.util

import org.apache.sis.geometry.DirectPosition2D
import org.apache.sis.index.tree.QuadTreeData
import org.mitre.synthea.distributed.world.agents.Person.BIRTHDATE
import org.mitre.synthea.distributed.world.agents.Provider.ProviderType
import org.mitre.synthea.engine.Generator.GeneratorOptions
import org.mitre.synthea.engine.{Event, EventList, Module, State}
import org.mitre.synthea.world.concepts.{HealthRecord, VitalSign}
import org.mitre.synthea.world.geography.Location

import scala.collection.{concurrent, mutable}
import scala.util.Random

case class PersonName(
                       FIRST_NAME: String,
                       LAST_NAME: String,
                       MAIDEN_NAME: String,
                       NAME_PREFIX: String,
                       NAME_SUFFIX: String,
                       NAME: String
                     )

case class BirthPlace(
                       ADDRESS: String,
                       CITY: String,
                       STATE: String,
                       ZIP: String,
                       BIRTHPLACE: String
                     )

object Person {
  type PreferredProviders = mutable.Map[ProviderType, Provider]

  type BIRTHDATE = Option[String]
  type RACE = String
  type ETHNICITY = String
  type FIRST_LANGUAGE = String
  type GENDER = String
  type MULTIPLE_BIRTH_STATUS = String
  type TELECOM = String
  type ID = String
  type COORDINATE = String
  type NAME_MOTHER = String
  type NAME_FATHER = String
  type MARITAL_STATUS = String
  type SOCIOECONOMIC_SCORE = String
  type SOCIOECONOMIC_CATEGORY = String
  type INCOME = String
  type INCOME_LEVEL = String
  type EDUCATION = String
  type EDUCATION_LEVEL = String
  type OCCUPATION_LEVEL = String
  type SMOKER = String
  type ALCOHOLIC = String
  type ADHERENCE = String
  type IDENTIFIER_SSN = String
  type IDENTIFIER_DRIVERS = String
  type IDENTIFIER_PASSPORT = String
  type CAUSE_OF_DEATH = String
  type SEXUAL_ORIENTATION = String
  type LOCATION = String
  type CURRENT_ENCOUNTERS = String
  // Providers API -----------------------------------------------------------
}

@SerialVersionUID(4322116644425686379L)
case class Person(seed: Long,
                  birthdate: BIRTHDATE,
                 )
  extends Serializable with QuadTreeData {

  import Person._

  /* state */
  var providers: PreferredProviders
  var vitalSigns: mutable.Map[VitalSign, Double]
  var symptoms: mutable.Map[String, mutable.Map[String, Int]]
  var symptomStatuses: mutable.Map[String, mutable.Map[String, Boolean]]
  var events: EventList
  var record: HealthRecord
  /** history of the currently active module. */
  var history: List[State]
  var encounters: mutable.Map[String, HealthRecord#Encounter]
  var location: Location
  var coord: DirectPosition2D
  var demoAttributes: concurrent.Map[String, Object]
  var options: GeneratorOptions
  var timeStep: Long
  var causeOfDeath: Option[HealthRecord.Code]
  val random = new Random(seed)
  var populationSeed = 0L


  def rand: Double = random.nextDouble

  def rand(low: Double, high: Double): Double = low + ((high - low) * random.nextDouble)

  /**
    * Helper function to get a random number based on an array of [min, max].
    * This should be used primarily when pulling ranges from YML.
    *
    * @param range array [min, max]
    * @return random double between min and max
    */
  def rand(range: Array[Double]): Double = {
    if (range == null || range.length != 2) throw new IllegalArgumentException("input range must be of length 2 -- got " + util.Arrays.toString(range))
    if (range(0) > range(1)) throw new IllegalArgumentException("range must be of the form {low, high} -- got " + util.Arrays.toString(range))
    rand(range(0), range(1))
  }

  /**
    * Return one of the options randomly with uniform distribution.
    *
    * @param choices The options to be returned.
    * @return One of the options randomly selected.
    */
  def rand(choices: Array[String]) = choices(random.nextInt(choices.length))

  /**
    * Helper function to get a random number based on an integer array of [min, max].
    * This should be used primarily when pulling ranges from YML.
    *
    * @param range array [min, max]
    * @return random double between min and max
    */
  def rand(range: Array[Int]): Double = {
    if (range == null || range.length != 2) throw new IllegalArgumentException("input range must be of length 2 -- got " + util.Arrays.toString(range))
    if (range(0) > range(1)) throw new IllegalArgumentException("range must be of the form {low, high} -- got " + util.Arrays.toString(range))
    rand(range(0), range(1))
  }

  def randInt: Int = random.nextInt

  def randInt(bound: Int): Int = random.nextInt(bound)

  def age(time: Long): Period =
    birthdate match {
      case Some(d) =>
        val now = Instant.ofEpochMilli(time).atZone(ZoneId.systemDefault).toLocalDate
        val birthdate = Instant.ofEpochMilli(d.asInstanceOf[Long]).atZone(ZoneId.systemDefault).toLocalDate
        Period.between(birthdate, now)
      case None =>
        Period.ZERO
    }

  def ageInMonths(time: Long): Int = age(time).toTotalMonths.toInt

  def ageInYears(time: Long): Int = age(time).getYears

  def alive(time: Long): Boolean = events.event(Event.BIRTH) != null && events.before(time, Event.DEATH).isEmpty

  def setSymptom(cause: String, symptomType: String, value: Int, addressed: Boolean): Unit = {
    symptoms.update(symptomType, mutable.Map(cause -> value))
    symptomStatuses.update(symptomType, mutable.Map(cause -> addressed))
  }

  // TODO: clean this up
  def getSymptom(symptomType: String): Int = (symptoms.get(symptomType), symptomStatuses.get(symptomType)) match {
    case (Some(typedSymptoms), Some(typedStatuses)) =>
      typedSymptoms.foldLeft(0){ case (acc, (cause, count)) => math.max(acc, count)}
    case _ => 0
  }

  //Mark the largest valued symptom as addressed.
  def addressLargestSymptom(): Unit = {
    var highestType = ""
    var highestCause = ""
    var maxValue = 0
    for (symptomType <- symptoms.keySet) {
      if (symptoms.contains(symptomType) && symptomStatuses.contains(symptomType)) {
        val typedSymptoms = symptoms.get(symptomType)
        for (cause <- typedSymptoms) {
          if (typedSymptoms.get(cause) > maxValue && !symptomStatuses.get(symptomType).get(cause)) {
            maxValue = typedSymptoms.get(cause)
            highestCause = cause
            highestType = symptomType
          }
        }
      }
    }
    symptomStatuses.get(highestType).put(highestCause, true)
  }

  def getVitalSign(vitalSign: VitalSign): Double = vitalSigns.getOrElse(vitalSign,
    throw new Exception(s"Vital sign '$vitalSign' undefined")
  )

  def setVitalSign(vitalSign: VitalSign, value: Double): Unit =
    vitalSigns.update(vitalSign, value)

  def recordDeath(time: Long, cause: HealthRecord.Code, ruleName: String): Unit = {
    events.create(time, Event.DEATH, ruleName, true)
    if (record.death == null || record.death > time) { // it's possible for a person to have a death date in the future
      // (ex, a condition with some life expectancy sets a future death date)
      // but then the patient dies sooner because of something else
      record.death = time
      if (cause == null) {
        causeOfDeath = None
      } else {
        causeOfDeath = Some(cause)
      }
    }
  }

  /**
    * The total number of all unaddressed symptom severities.
    *
    * @return total : sum of all the symptom severities. This number drives care-seeking behaviors.
    */
  def symptomTotal: Int = {
    var total = 0
    for (symptomType <- symptoms.keySet) {
      total += getSymptom(symptomType)
    }
    total
  }

  def resetSymptoms(): Unit = {
    symptoms.clear()
  }

  def hadPriorState(name: String): Boolean = hadPriorState(name, null, null)

  def hadPriorState(name: String, since: String, within: Long): Boolean = {
    if (history.isEmpty) {
      false
    } else {
      for (state <- history) {
        if (within != null && state.exited != null && state.exited <= within) return false
        if (since != null && state.name == since) return false
        if (state.name == name) return true
      }
      false

    }
  }


  def getProvider(encounterClass: String, time: Long): Provider = {

    providers.getOrElseUpdate(encounterClass,
      Provider.findClosestService(this, encounterClass, time)
    )

  }

  /*
     * (non-Javadoc)
     * @see org.apache.sis.index.tree.QuadTreeData#getX()
     */
  override def getX: Double = getLatLon.getX

  /*
     * (non-Javadoc)
     * @see org.apache.sis.index.tree.QuadTreeData#getY()
     */
  override def getY: Double = getLatLon.getY

  /*
     * (non-Javadoc)
     * @see org.apache.sis.index.tree.QuadTreeData#getLatLon()
     */
  override def getLatLon: DirectPosition2D = coord

  /*
     * (non-Javadoc)
     * @see org.apache.sis.index.tree.QuadTreeData#getFileName()
     */
  override def getFileName: String = null
}

