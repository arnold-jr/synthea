package org.mitre.synthea.distributed.modules

import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import java.util
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging, ActorRef}
import org.apache.commons.math3.special.Erf
import org.mitre.synthea.distributed.Protocol.AdvanceOneTimeStep
import org.mitre.synthea.distributed.world.agents.Protocol.BeBorn
import org.mitre.synthea.distributed.world.agents.{Gender, Person}
import org.mitre.synthea.distributed.world.agents.Person.BirthPlace
import org.mitre.synthea.helpers.{Config, SimpleYML, Utilities}
import org.mitre.synthea.distributed.world.agents.Person
import org.mitre.synthea.world.concepts.{BiometricsConfig, HealthRecord, VitalSign}
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
  def age(data: Data)(time: Long): Data = {

    val newAge = data.age.plusDays(time)


    val data1: Data = newAge.getYears match {
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

  private val HYPERTENSIVE_SYS_BP_RANGE = BiometricsConfig.ints("metabolic.blood_pressure.hypertensive.systolic")
  private val HYPERTENSIVE_DIA_BP_RANGE = BiometricsConfig.ints("metabolic.blood_pressure.hypertensive.diastolic")
  private val NORMAL_SYS_BP_RANGE = BiometricsConfig.ints("metabolic.blood_pressure.normal.systolic")
  private val NORMAL_DIA_BP_RANGE = BiometricsConfig.ints("metabolic.blood_pressure.normal.diastolic")

  private val CHOLESTEROL_RANGE = BiometricsConfig.ints("metabolic.lipid_panel.cholesterol")
  private val TRIGLYCERIDES_RANGE = BiometricsConfig.ints("metabolic.lipid_panel.triglycerides")
  private val HDL_RANGE = BiometricsConfig.ints("metabolic.lipid_panel.hdl")

  private val GLUCOSE_RANGE = BiometricsConfig.ints("metabolic.basic_panel.glucose")

  private val UREA_NITROGEN_RANGE = BiometricsConfig.ints("metabolic.basic_panel.normal.urea_nitrogen")
  private val CALCIUM_RANGE = BiometricsConfig.doubles("metabolic.basic_panel.normal.calcium")


  private val MILD_KIDNEY_DMG_CC_RANGE = BiometricsConfig.ints("metabolic.basic_panel.creatinine_clearance.mild_kidney_damage")
  private val MODERATE_KIDNEY_DMG_CC_RANGE = BiometricsConfig.ints("metabolic.basic_panel.creatinine_clearance.moderate_kidney_damage")
  private val SEVERE_KIDNEY_DMG_CC_RANGE = BiometricsConfig.ints("metabolic.basic_panel.creatinine_clearance.severe_kidney_damage")
  private val ESRD_CC_RANGE = BiometricsConfig.ints("metabolic.basic_panel.creatinine_clearance.esrd")
  private val NORMAL_FEMALE_CC_RANGE = BiometricsConfig.ints("metabolic.basic_panel.creatinine_clearance.normal.female")
  private val NORMAL_MALE_CC_RANGE = BiometricsConfig.ints("metabolic.basic_panel.creatinine_clearance.normal.male")

  private val NORMAL_MCR_RANGE = BiometricsConfig.ints("metabolic.basic_panel.microalbumin_creatinine_ratio.normal")
  private val CONTROLLED_MCR_RANGE = BiometricsConfig.ints("metabolic.basic_panel.microalbumin_creatinine_ratio.microalbuminuria_controlled")

  private val UNCONTROLLED_MCR_RANGE = BiometricsConfig.ints("metabolic.basic_panel.microalbumin_creatinine_ratio.microalbuminuria_uncontrolled")

  private val PROTEINURIA_MCR_RANGE = BiometricsConfig.ints("metabolic.basic_panel.microalbumin_creatinine_ratio.proteinuria")

  private val CHLORIDE_RANGE = BiometricsConfig.doubles("metabolic.basic_panel.normal.chloride")
  private val POTASSIUM_RANGE = BiometricsConfig.doubles("metabolic.basic_panel.normal.potassium")
  private val CO2_RANGE = BiometricsConfig.doubles("metabolic.basic_panel.normal.carbon_dioxide")
  private val SODIUM_RANGE = BiometricsConfig.doubles("metabolic.basic_panel.normal.sodium")


  def grow(data: Data): Data = {

    // TODO: Incorporate this logic somehow
    /*
    boolean shouldGrow;
    if (newAge >= 20) {
      // adults 20 and over grow once per year
      shouldGrow = (newAge > prevAge);
    } else {
      // people under 20 grow once per month
      shouldGrow = (newAgeMos > prevAgeMos);
   */

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

  private def startSmoking(data: Data)(time: Long): Data = { // 9/10 smokers start before age 18. We will use 16.


    def likelihoodOfBeingASmoker(year: Int): Double = { // 16.1% of MA are smokers in 2016.
    // http://www.cdc.gov/tobacco/data_statistics/state_data/state_highlights/2010/states/massachusetts/
    // but the rate is decreasing over time
    // http://www.cdc.gov/tobacco/data_statistics/tables/trends/cig_smoking/
    // selected #s:
    // 1965 - 42.4%
    // 1975 - 37.1%
    // 1985 - 30.1%
    // 1995 - 24.7%
    // 2005 - 20.9%
    // 2015 - 16.1%
    // assume that it was never significantly higher than 42% pre-1960s, but will continue to drop
    // slowly after 2016
    // it's decreasing about .5% per year
    if (year < 1965) return 0.424
    ((year * -0.4865) + 996.41) / 100.0
  }

    // http://www.cdc.gov/tobacco/data_statistics/fact_sheets/youth_data/tobacco_use/
    if (data.behavior.smoker.isEmpty && data.age.getYears == 16) {
      val year = Utilities.getYear(time)
      if (data.random.nextFloat < likelihoodOfBeingASmoker(year)) {
        data.copy(
          behavior =
            data.behavior.copy(
              smoker = Some(true),
              quitSmokingProbability = Some(Config.get("lifecycle.quit_smoking.baseline", "0.01").toDouble)
            )
        )
      } else {
        data
      }
    } else {
      data
    }
  }

  private def startAlcoholism(data: Data): Data = { // TODO there are various types of alcoholics with different characteristics
    // including age of onset of dependence. we pick 25 as a starting point
    // https://www.therecoveryvillage.com/alcohol-abuse/types-alcoholics/
    if (data.behavior.alcoholic.isEmpty && data.age.getYears == 25) { // TODO assume about 8 mil alcoholics/320 mil gen pop
      val alcoholic = data.random.nextFloat < 0.025
      val quitAlcoholismBaseline = Config.get("lifecycle.quit_alcoholism.baseline", "0.05").toDouble
      data.copy(
        behavior = data.behavior.copy(
          alcoholic = Some(alcoholic),
          quitAlcoholismProbability = Some(quitAlcoholismBaseline)
        )
      )
    } else {
      data
    }
  }

  def quitSmoking(data: Data)(time: Long): Data = data.behavior.smoker match {

    case Some(true) =>
      var probability = data.behavior.quitSmokingProbability.get

      if (data.random.nextFloat < probability) {
        data.copy(
          behavior =
            data.behavior.copy(
              smoker = Some(false),
              quitSmokingAge = Some(data.age.getYears)
            )
        )
      } else {
        val quitSmokingBaseline = Config.get("lifecycle.quit_smoking.baseline", "0.01").toDouble
        val quitSmokingTimestepDelta = Config.get("lifecycle.quit_smoking.timestep_delta", "-0.1").toDouble
        probability += quitSmokingTimestepDelta
        if (probability < quitSmokingBaseline) probability = quitSmokingBaseline
        data.copy(
          behavior =
            data.behavior.copy(
              quitSmokingProbability = Some(probability)
            )
        )
      }

    case _ => data
  }

  def quitAlcoholism(data: Data): Data = data.behavior.alcoholic match {
    case Some(true) =>
      var probability = data.behavior.quitAlcoholismProbability.get
      if (data.random.nextFloat < probability) {
        data.copy(
          behavior =
            data.behavior.copy(
              alcoholic = Some(false),
              quitAlcoholismAge = Some(data.age.getYears)
            )
        )
      } else {
        val quitAlcoholismBaseline = Config.get("lifecycle.quit_alcoholism.baseline", "0.01").toDouble
        val quitAlcoholismTimestepDelta = Config.get("lifecycle.quit_alcoholism.timestep_delta", "-0.1").toDouble
        probability += quitAlcoholismTimestepDelta
        if (probability < quitAlcoholismBaseline) probability = quitAlcoholismBaseline
        data.copy(
          behavior =
            data.behavior.copy(
              quitAlcoholismProbability = Some(probability)
            )
        )
      }
    case _ => data
  }

  private def adherence(data: Data): Data = {
    if (data.behavior.adherence.isDefined) {
      var probability = data.behavior.adherenceProbability.get
      val adherenceBaseline = Config.get("lifecycle.adherence.baseline", "0.05").toDouble
      val adherenceTimestepDelta = Config.get("lifecycle.aherence.timestep_delta", "-.01").toDouble
      probability += adherenceTimestepDelta
      if (probability < adherenceBaseline) probability = adherenceBaseline
      data.copy(
        behavior =
          data.behavior.copy(
            adherenceProbability = Some(probability)
          )
      )
    } else {
      data
    }
  }

  /**
    * Calculate this person's vital signs,
    * based on their conditions, medications, body composition, etc.
    *
    * @param person The person
    * @param time   Current simulation timestamp
    */
  private def diabeticVitalSigns(person: Data): Data = {

    val fs: List[Data => Data] = List(
      setBloodPressure(_),
      setCholesterol(_),
      setHbA1c(_),
      setCreatinine(_),
      setGlucose(_)
    )

    fs.foldLeft(person)((p, f) => f(p))

  }


  def setBloodPressure(person: Data): Data = {
    val hypertension = person.conditions.hypertension.getOrElse(false)

    val (sysRange, diaRange) = if (hypertension) {
      (HYPERTENSIVE_SYS_BP_RANGE, HYPERTENSIVE_DIA_BP_RANGE)
    } else {
      (NORMAL_SYS_BP_RANGE, NORMAL_DIA_BP_RANGE)
    }
    person.copy(
      vitalSigns = person.vitalSigns.copy(
        systolicBloodPressure = Some(person.random.nextDouble  * sysRange),
        diastolicBloodPressure = Some(person.random.nextDouble * diaRange)
      )
    )
  }


  def setCholesterol(person: Data): Data = {
    val index = person.conditions.diabetesSeverity match {
      case Some(x) => x
      case _ => 0
    }

    val totalCholesterol = person.rand(CHOLESTEROL_RANGE(index), CHOLESTEROL_RANGE(index + 1))
    val triglycerides = person.rand(TRIGLYCERIDES_RANGE(index), TRIGLYCERIDES_RANGE(index + 1))
    val hdl = person.rand(HDL_RANGE(index), HDL_RANGE(index + 1))
    val ldl = totalCholesterol - hdl - (0.2 * triglycerides)

    person.copy(
      vitalSigns = person.vitalSigns.copy(
        totalCholesterol = Some(totalCholesterol),
        triglycerides = Some(triglycerides),
        hdl = Some(hdl),
        ldl = Some(ldl)
      )
    )
  }


  /**
    * Estimate the person's HbA1c using BMI and whether or not they have diabetes or prediabetes as a
    * rough guideline.
    *
    * @param prediabetes
    * Whether or not the person is prediabetic. (Diagnosed or undiagnosed)
    * @param diabetes
    * Whether or not the person is diabetic. (Diagnosed or undiagnosed)
    * @param p
    * The person
    * @return A calculated HbA1c value.
    */
  def setHbA1c(person: Data): Data = {
    val bmi = person.vitalSigns.bmi
    val prediabetes = person.conditions.prediabetes.getOrElse(false)
    val diabetes = person.conditions.diabetes.getOrElse(false)

    var hbA1c = (diabetes, prediabetes, bmi) match {
      case (true, _, Some(bmi1)) =>
        if (bmi1 > 48.0) {
          12.0
        }
        else if (bmi1 <= 27.0) {
          6.6
        }
        else {
          bmi1 / 4.0
          // very simple BMI function so that BMI 40 --> blood glucose ~ 10,
          // but with a bounded min at 6.6 and bounded max at 12.0
        }
      case (false, true, _) =>
        person.rand(Array(5.8, 6.4))

      case _ =>
        person.rand(Array(5.0, 5.7))
    }

    if (prediabetes || diabetes) { // drugs reduce hbA1c.
      // only do this for people that have pre/diabetes,
      // because these drugs are only prescribed if they do
      import scala.collection.JavaConversions._

      for (e <- DIABETES_DRUG_HBA1C_IMPACTS.entrySet) {
        val medicationCode = e.getKey
        val impact = e.getValue
        if (person.record.medicationActive(medicationCode)) { // impacts are negative, so add them
          hbA1c += impact
        }
      }
    }

    person.copy(
      vitalSigns = person.vitalSigns.copy(
        bloodGlucose = Some(hbA1c)
      )
    )

  }

  /**
    * Calculate Creatinine from Creatinine Clearance.
    * Source: http://www.mcw.edu/calculators/creatinine.htm
    *
    * @param person The person
    * @param crcl   Creatinine Clearance
    * @param time   Current Time
    * @return Estimated Creatinine
    */
  def setCreatinine(person: Data): Data = {

    val (ccRange, mcrRange) = person.conditions.diabeticKidneyDamage match {
      case Some(1) =>
        (MILD_KIDNEY_DMG_CC_RANGE, NORMAL_MCR_RANGE)
      case Some(2) =>
        (MODERATE_KIDNEY_DMG_CC_RANGE, CONTROLLED_MCR_RANGE)
      case Some(3) =>
        (SEVERE_KIDNEY_DMG_CC_RANGE, UNCONTROLLED_MCR_RANGE)
      case Some(4) =>
        (ESRD_CC_RANGE, PROTEINURIA_MCR_RANGE)
      case _ =>
        (
          if ("F" == person.gender) NORMAL_FEMALE_CC_RANGE else NORMAL_MALE_CC_RANGE,
          NORMAL_MCR_RANGE
        )
    }

    val creatinineClearance = person.rand(ccRange)
    val creatinine = {
      val age = person.age.getYears
      val weight = person.vitalSigns.weight.get // kg
      val crclEffective = math.max(1, Math.min(creatinineClearance, 100)) // clamp between 1-100

      val cr = ((140.0 - age) * weight) / (72.0 * crclEffective)

      if ("F" == person.gender) {
        cr * 0.85
      } else {
        1.0
      }

    }

    person.copy(
      vitalSigns = person.vitalSigns.copy(
        egfr = Some(creatinineClearance),
        microalbuminCreatinineRatio = Some(person.rand(mcrRange)),
        creatinine = Some(creatinine),
        ureaNitrogen = Some(person.rand(UREA_NITROGEN_RANGE)),
        calcium = Some(person.rand(CALCIUM_RANGE))
      )
    )

  }

  def setGlucose(person: Data): Data = {

    val index = person.conditions.diabetesSeverity match {
      case Some(x) => math.min(x, 2)
      case _ => 0
    }

    person.copy(
      vitalSigns = person.vitalSigns.copy(
        glucose = Some(person.rand(GLUCOSE_RANGE(index), GLUCOSE_RANGE(index + 1))),
        chloride = Some(person.rand(CHLORIDE_RANGE)),
        potassium = Some(person.rand(POTASSIUM_RANGE)),
        carbonDioxide = Some(person.rand(CO2_RANGE)),
        sodium = Some(person.rand(SODIUM_RANGE))
      )
    )
  }


  // referenced in the Injuries module - adults > age 65 have multiple screenings that affect fall
  // risk
  private def calculateFallRisk(person: Data): Data =
    if (person.age.getYears >= 65) {
      val hasOsteoporosis = person.conditions.osteoporosis.getOrElse(false)
      val baselineFallRisk = if (hasOsteoporosis) 0.06
      else 0.035
      // numbers from injuries module
      var activeInterventions = 0
      // careplan for exercise or PT
      if (person.record.careplanActive("Physical therapy") || person.record.careplanActive("Physical activity target light exercise")) activeInterventions += 1
      // taking vitamin D
      if (person.record.medicationActive("Cholecalciferol 600 UNT")) activeInterventions += 1
      // osteoporosis diagnosis makes them more careful
      if (person.record.conditionActive("Osteoporosis (disorder)")) activeInterventions += 1
      val fallRisk = baselineFallRisk * (1 - 0.02 * activeInterventions)
      // reduce the fall risk by 2% per intervention
      // TODO - research actual effectiveness of these interventions
      person.copy(
        conditions = person.conditions.copy(
          probabilityOfFallInjury = Some(fallRisk)
        )
      )
    }

  private val ENABLE_DEATH_BY_NATURAL_CAUSES = Config.get("lifecycle.death_by_natural_causes").asInstanceOf[Boolean]

  private val NATURAL_CAUSES = new HealthRecord.Code("SNOMED-CT", "9855000", "Natural death with unknown cause")

  private def death(person: Data)(time: Long): Data = {
    if (ENABLE_DEATH_BY_NATURAL_CAUSES) {
      val roll = person.random.nextDouble
      val likelihoodOfDeath = likelihoodOfDeath(person.age.getYears)
      if (roll < likelihoodOfDeath) {
        // recordDeath(time, NATURAL_CAUSES, "death")
        person.copy(
          legal = person.legal.copy(
            timeOfDeath = Some(LocalDateTime.ofEpochSecond(time, 0, ZoneOffset.UTC))
          )
        )
      } else {
        person
      }
    } else {
      person
    }
  }

  private def likelihoodOfDeath(age: Int): Double = {
    val yearlyRisk =
      if (age < 1) 508.1 / 100000.0
      else if (age >= 1 && age <= 4) 15.6 / 100000.0
      else if (age >= 5 && age <= 14) 10.6 / 100000.0
      else if (age >= 15 && age <= 24) 56.4 / 100000.0
      else if (age >= 25 && age <= 34) 74.7 / 100000.0
      else if (age >= 35 && age <= 44) 145.7 / 100000.0
      else if (age >= 45 && age <= 54) 326.5 / 100000.0
      else if (age >= 55 && age <= 64) 737.8 / 100000.0
      else if (age >= 65 && age <= 74) 1817.0 / 100000.0
      else if (age >= 75 && age <= 84) 4877.3 / 100000.0
      else if (age >= 85 && age <= 94) 13499.4 / 100000.0
      else 50000.0 / 100000.0

    val oneYearInMs = TimeUnit.DAYS.toMillis(365)
    val adjustedRisk = Utilities.convertRiskToTimestep(yearlyRisk, oneYearInMs)
    adjustedRisk
  }

  def process(data: Data, time: Long): Data = { // run through all of the rules defined
    // ruby "rules" are converted to static functions here
    // since this is intended to only be temporary
    // birth(person, time); intentionally left out - call it only once from Generator
    //if (age(person, time)) grow(person, time)

    val fs: List[Data => Data] = List(
      startSmoking(_)(time),
      startAlcoholism(_),
      quitSmoking(_)(time),
      quitAlcoholism(_),
      adherence(_),
      diabeticVitalSigns(_),
      calculateFallRisk(_),
      death(_)(time)
    )

    fs.foldLeft(data)((d, f) => f(d))

  }
}
