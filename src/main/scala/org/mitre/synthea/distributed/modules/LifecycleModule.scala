package org.mitre.synthea.distributed.modules

import java.time.{LocalDate, Period}
import java.util

import akka.actor.{Actor, ActorLogging, ActorRef}
import com.google.gson.Gson
import org.mitre.synthea.distributed.Protocol.AdvanceOneTimeStep
import org.mitre.synthea.distributed.world.agents.Protocol.BeBorn
import org.mitre.synthea.distributed.world.agents.{BirthPlace, Person, VitalSigns}
import org.mitre.synthea.helpers.Utilities
import org.mitre.synthea.world.concepts.VitalSign

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

  private def bmi(heightCM: Double, weightKG: Double) = weightKG / ((heightCM / 100.0) * (heightCM / 100.0))
  def grow(data: StateData): StateData = {

    val age = data.age
    val vitalSigns = data.vitalSigns

    val height = vitalSigns.height.get
    val weight = vitalSigns.weight.get

    val (newHeight, newWeight) =
    if (age.getYears < 20) { // follow growth charts
      val gender = data.gender
      val ageInMonths = age.getMonths
      (lookupGrowthChart("height", gender, ageInMonths, vitalSigns.heightPercentile.get),
      lookupGrowthChart("weight", gender, ageInMonths, vitalSigns.weightPercentile.get))
    }
    else if (age <= ADULT_MAX_WEIGHT_AGE) { // getting older and fatter
      val adultWeightGain = person.rand(ADULT_WEIGHT_GAIN_RANGE)
      (height, weight + adultWeightGain)
    }
    else if (age >= GERIATRIC_WEIGHT_LOSS_AGE) { // getting older and wasting away
      val geriatricWeightLoss = person.rand(GERIATRIC_WEIGHT_LOSS_RANGE)
      (height, weight - geriatricWeightLoss)
    }

    data.copy(vitalSigns = vitalSigns.copy(
      height = Some(newHeight),
      weight = Some(newWeight),
      bmi = Some(bmi(newHeight, newWeight))
    ))
  }

  private def bmi(heightCM: Double, weightKG: Double) = weightKG / ((heightCM / 100.0) * (heightCM / 100.0))

  val growthChart: util.HashMap[_, _]= {
    val filename = "cdc_growth_charts.json"
    try {
      val json = Utilities.readResource(filename)
      val g = new Gson
      g.fromJson(json, classOf[util.HashMap[_, _]])
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
    val chart = growthChart.get(heightOrWeight).asInstanceOf[Map[_, _]]
    val byGender = chart.get(gender).asInstanceOf[Map[_, _]]
    val byAge = byGender.get(Integer.toString(ageInMonths)).asInstanceOf[Map[_, _]]
    val l = byAge.get("l").asInstanceOf[String].toDouble
    val m = byAge.get("m").asInstanceOf[String].toDouble
    val s = byAge.get("s").asInstanceOf[String].toDouble
    val z = calculateZScore(percentile)
    if (l == 0) m * Math.exp(s * z)
    else m * Math.pow(1 + (l * s * z), 1.0 / l)
  }

}
