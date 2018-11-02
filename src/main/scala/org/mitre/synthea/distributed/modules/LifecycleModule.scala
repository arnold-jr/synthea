package org.mitre.synthea.distributed.modules

import java.time.LocalDate
import java.util

import akka.actor.{Actor, ActorLogging, ActorRef}
import org.apache.commons.math3.special.Erf
import org.mitre.synthea.distributed.Protocol.AdvanceOneTimeStep
import org.mitre.synthea.distributed.world.agents.Protocol.BeBorn
import org.mitre.synthea.distributed.world.agents.{Gender, Person}
import org.mitre.synthea.distributed.world.agents.Person.BirthPlace
import org.mitre.synthea.helpers.SimpleYML
import org.mitre.synthea.world.concepts.BiometricsConfig
import spray.json._

import scala.collection.Map
import scala.util.Random

class LifecycleModule(var today: LocalDate) extends Actor with ActorLogging {

  var people: Vector[ActorRef] = Vector.empty[ActorRef]

  override def receive: Receive = {
    case AdvanceOneTimeStep =>
      createPeople()
      advancePeople()
    case _ =>
      log.info("I got a message!")
  }

  def createPeople(): Unit = {
    val newPeople = Vector.fill[ActorRef](1){
      context.actorOf(Person.props(0L,
        BirthPlace("1234 Any Drive", "Anytown", "AL", "12345", "Anytown"),
        Some(today)))
    }

    newPeople.foreach(a => a ! BeBorn)
    people = people ++ newPeople
  }

  def advancePeople(): Unit = {
    people.foreach(a => a ! AdvanceOneTimeStep)
  }

}

object LifecycleModule {

  import Person._

  /**
    * Age the patient.
    *
    */
  def age(data: StateData, time: Long): StateData = {

    val newAge = data.age.plusDays(time)


    val data1: StateData = newAge.getYears match {
      case 16 =>
        // driver's license
        data.legal.driversId match {
          case Some(_) => data
          case None =>
            val identifierDrivers = "S999" + data.random.nextInt(99999 - 10000 + 1) + 10000
            data.copy(legal = data.legal.copy(driversId = Some(identifierDrivers)))
        }
      case 18 =>
        // name prefix
        data.name.namePrefix match {
          case Some(_) => data
          case None =>
            val namePrefix =
              if ("M" == data.gender) {
                "Mr."
              } else {
                "Ms."
              }
            data.copy(name = data.name.copy(namePrefix = Some(namePrefix)))
        }
      case 20 =>
        // passport number
        data.legal.passportId match {
          case Some(_) => data
          case None =>
            val getsPassport = data.random.nextFloat < 0.5
            if (getsPassport) {
              val identifierPassport = s"X${data.random.nextInt(10000000) + 10000000}X"
              data.copy(legal = data.legal.copy(passportId = Some(identifierPassport)))
            } else {
              data
            }
        }
      case 27 =>
        // get married
        data.legal.maritalStatus match {
          case Some(_) => data
          case None =>
            val getsMarried = data.random.nextFloat < 0.8
            if (getsMarried) {
              val newStatus = "M"

              if ("F" == data.gender) {
                val language = data.socioEconomic.firstLanguage.getOrElse("english")
                data.copy(
                  legal = data.legal.copy(
                    maritalStatus = Some(newStatus)
                  ),
                  name = data.name.copy(
                    namePrefix = Some("Mrs."),
                    maidenName = Some(data.name.lastName),
                    lastName = fakeLastName(language, data.random)
                  )
                )

              }
              else {
                data.copy(legal = data.legal.copy(maritalStatus = Some(newStatus)))
              }
            }
            else {
              data.copy(legal = data.legal.copy(maritalStatus = Some("S")))
            }
        }
      case 30 =>
        // "overeducated" -> suffix
        (data.name.nameSuffix, data.socioEconomic.educationLevel) match {
          case (None, Some(v)) if v >= 0.95 =>
            val suffixList = List("PhD", "JD", "MD")
            data.copy(
              name = data.name.copy(
                nameSuffix = Some(suffixList(data.random.nextInt(suffixList.length)))
              )
            )
          case _ => data
        }
      case _ => data
    }

    data1.copy(age = newAge)

  }

  private val ADULT_MAX_WEIGHT_AGE = BiometricsConfig.get("lifecycle.adult_max_weight_age", 49).asInstanceOf[Int]

  private val GERIATRIC_WEIGHT_LOSS_AGE = BiometricsConfig.get("lifecycle.geriatric_weight_loss_age", 60).asInstanceOf[Int]

  private val ADULT_WEIGHT_GAIN_RANGE = BiometricsConfig.doubles("lifecycle.adult_weight_gain")

  private val GERIATRIC_WEIGHT_LOSS_RANGE = BiometricsConfig.doubles("lifecycle.geriatric_weight_loss")

  def grow(data: StateData): StateData = {

    val age = data.age
    val vitalSigns = data.vitalSigns
    val rand = data.random

    val height = vitalSigns.height.get
    val weight = vitalSigns.weight.get

    val (newHeight, newWeight): (Double, Double) =
      if (age.getYears < 20) { // follow growth charts
        val gender = data.gender
        val ageInMonths = age.getMonths
        (lookupGrowthChart("height", gender, ageInMonths, vitalSigns.heightPercentile.get),
          lookupGrowthChart("weight", gender, ageInMonths, vitalSigns.weightPercentile.get))
      }
      else if (age.getYears <= ADULT_MAX_WEIGHT_AGE) { // getting older and fatter
        val adultWeightGain = rand.nextDouble() * (ADULT_WEIGHT_GAIN_RANGE(1) - ADULT_WEIGHT_GAIN_RANGE(0))
        (height, weight + adultWeightGain)
      }
      else if (age.getYears >= GERIATRIC_WEIGHT_LOSS_AGE) { // getting older and wasting away
        val geriatricWeightLoss = rand.nextDouble * (GERIATRIC_WEIGHT_LOSS_RANGE(1) - GERIATRIC_WEIGHT_LOSS_RANGE(0))
        (height, weight - geriatricWeightLoss)
      }

    data.copy(vitalSigns = vitalSigns.copy(
      height = Some(newHeight),
      weight = Some(newWeight),
      bmi = Some(bmi(newHeight, newWeight))
    ))
  }

  def bmi(heightCM: Double, weightKG: Double): Double = weightKG / ((heightCM / 100.0) * (heightCM / 100.0))


  type GrowthChart =
    Map[String,
      Map[String,
        Map[String,
          Map[String, String]
          ]
        ]
      ]


  object GrowthChartJsonProtocol extends DefaultJsonProtocol {

    implicit object GrowthChartJsonFormat extends RootJsonFormat[GrowthChart] {

      def write(gw: GrowthChart): JsValue =
        JsString("not implemented")

      def read(value: JsValue): GrowthChart =
        value.asJsObject.fields.mapValues(
          v1 => v1.asJsObject.fields.mapValues(
            v2 => v2.asJsObject.fields.mapValues(
              v3 => v3.asJsObject.fields.mapValues {
                case JsString(s) => s
                case _ => throw new Exception("undefined depth")
              }
            )
          )
        )
    }

  }

  private lazy val names: SimpleYML = {
    val filename = "names.yml"
    try {
      val namesData = io.Source.fromResource(filename).getLines().mkString("\\n")
      new SimpleYML(namesData)
    } catch {
      case e: Exception =>
        System.err.println("ERROR: unable to load yml: " + filename)
        e.printStackTrace()
        throw new ExceptionInInitializerError(e)
    }
  }


  val growthChart: Map[String, Map[String, Map[String, Map[String, String]]]] = {

    import GrowthChartJsonProtocol._

    val filename = "cdc_growth_charts.json"
    try {
      val source = scala.io.Source.fromResource(filename).getLines().mkString
      val jsonAst = source.parseJson
      jsonAst.convertTo[GrowthChart]
    } catch {
      case e: Exception =>
        System.err.println("ERROR: unable to load json: " + filename)
        e.printStackTrace()
        throw new ExceptionInInitializerError(e)
    }
  }

  /**
    * Lookup and calculate values from the CDC growth charts, using the LMS
    * values to calculate the intermediate values.
    * Reference : https://www.cdc.gov/growthcharts/percentile_data_files.htm
    *
    * @param heightOrWeight "height" | "weight"
    * @param gender         "M" | "F"
    * @param ageInMonths    0 - 240
    * @param percentile     0.0 - 1.0
    * @return The height (cm) or weight (kg)
    */
  def lookupGrowthChart(heightOrWeight: String, gender: String, ageInMonths: Int, percentile: Double): Double = {
    val chart = growthChart.get(heightOrWeight)
    val byGender = chart.get(gender)
    val byAge = byGender.get(ageInMonths.toString)
    val l = byAge.get("l").toDouble
    val m = byAge.get("m").toDouble
    val s = byAge.get("s").toDouble
    val z = calculateZScore(percentile)
    if (l == 0) m * Math.exp(s * z)
    else m * Math.pow(1 + (l * s * z), 1.0 / l)
  }

  /**
    * Z is the z-score that corresponds to the percentile.
    * z-scores correspond exactly to percentiles, e.g.,
    * z-scores of:
    * -1.881, // 3rd
    * -1.645, // 5th
    * -1.282, // 10th
    * -0.674, // 25th
    * 0,     // 50th
    *  0.674, // 75th
    *  1.036, // 85th
    *  1.282, // 90th
    *  1.645, // 95th
    *  1.881  // 97th
    *
    * @param percentile 0.0 - 1.0
    * @return z-score that corresponds to the percentile.
    */
  private def calculateZScore(percentile: Double): Double = { // Set percentile gt0 and lt1, otherwise the error
    // function will return Infinity.
    val adjusted =
    if (percentile >= 1.0) {
      0.999
    }
    else if (percentile <= 0.0) {
      0.001
    }
    else {
      percentile
    }

    -1 * Math.sqrt(2) * Erf.erfcInv(2 * adjusted)
  }

  def fakeFirstName(gender: Gender, language: String, random: Random): String = {
    val choices: List[String] = language.toLowerCase match {
      case "english" => names.get("english." + gender).asInstanceOf[List[String]]
      case "spanish" => names.get("spanish." + gender).asInstanceOf[List[String]]
      case _ => throw new IllegalArgumentException(s"Illegal language: $language")
    }
    choices(random.nextInt(choices.length))
  }

  def fakeLastName(language: String, random: Random): String = {
    val choices: List[String] = language.toLowerCase match {
      case "english" => names.get("english.family").asInstanceOf[List[String]]
      case "spanish" => names.get("spanish.family").asInstanceOf[List[String]]
      case _ => throw new IllegalArgumentException(s"Illegal language: $language")
    }
    choices(random.nextInt(choices.length))
  }

  def fakeAddress(includeLine2: Boolean, random: Random): String = {
    val number = random.nextInt(1000) + 100
    val n = names.get("english.family").asInstanceOf[util.List[String]]
    // for now just use family names as the street name.
    // could expand with a few more but probably not worth it
    val streetName = n.get(random.nextInt(n.size))
    val a = names.get("street.type").asInstanceOf[List[String]]
    val streetType = a(random.nextInt(a.size))
    if (includeLine2) {
      val addtlNum = random.nextInt(100)
      val s = names.get("street.secondary").asInstanceOf[List[String]]
      val addtlType = s(random.nextInt(s.size))
      number + " " + streetName + " " + streetType + " " + addtlType + " " + addtlNum
    }
    else number + " " + streetName + " " + streetType
  }

  /**
    * Adds a 1- to 3-digit hashcode to the end of the name.
    *
    * @param name Person's name
    * @return The name with a hash appended, ex "John123" or "Smith22"
    */
  def addHash(name: String): String = { // note that this value should be deterministic
    // It cannot be a random number. It needs to be a hash value or something deterministic.
    // We do not want John10 and John52 -- we want all the Johns to have the SAME numbers. e.g. All
    // people named John become John52
    // Why? Because we do not know how using systems will index names. Say a user of an system
    // loaded with Synthea data wants to find all the people named John Smith. This will be easier
    // if John Smith always resolves to John52 Smith32 and not [John52 Smith32, John10 Smith22, ...]
    name + Integer.toString(Math.abs(name.hashCode % 1000))
  }

}
