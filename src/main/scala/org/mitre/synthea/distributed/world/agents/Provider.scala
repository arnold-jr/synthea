package org.mitre.synthea.distributed.world.agents

import com.google.common.collect.HashBasedTable
import com.google.common.collect.Table
import com.google.gson.internal.LinkedTreeMap
import java.io.IOException
import java.util
import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{ActorRef, FSM}
import org.apache.sis.geometry.DirectPosition2D
import org.apache.sis.index.tree.QuadTree
import org.apache.sis.index.tree.QuadTreeData
import org.mitre.synthea.distributed.world.agents.Protocol.AdvanceToDateTime
import org.mitre.synthea.distributed.world.geography.Place
import org.mitre.synthea.helpers.Config
import org.mitre.synthea.helpers.SimpleCSV
import org.mitre.synthea.helpers.Utilities
import org.mitre.synthea.modules.LifecycleModule
import org.mitre.synthea.world.agents
import org.mitre.synthea.world.agents.{Clinician, Person, Provider}
import org.mitre.synthea.world.concepts.ClinicianSpecialty
import org.mitre.synthea.world.geography.Demographics
import org.mitre.synthea.world.geography.Location

import scala.util.Random


class Provider(place: Place) extends FSM[Provider.State, Provider.StateData] {
  import Provider._


  when(Hiring) {
    case Event("Find people to hire", _) =>
      stay
  }

  when(Open) {
    case Event("Treat patient", _) =>
      //if ("VA Facility" == this.providerType && !person.attributes.containsKey("veteran")) return false
      stay
  }

  /**
    * Generates a list of clinicians, given the number to generate and the specialty.
    *
    * @param numClinicians - the number of clinicians to generate
    * @param specialty     - which specialty clinicians to generate
    * @return
    */
  private def generateClinicianList(numClinicians: Int,
                                    specialty: ClinicianSpecialty,
                                    seed: Option[Long] = None): List[Clinician.Data] = {

    List.range(0, numClinicians).map{ i =>

      val clinicianSeed = seed match {
        case Some(s) => s
        case None => UUID.randomUUID.getMostSignificantBits & Long.MaxValue
      }

      val randomForDemographics = new Random(clinicianSeed)
      val city = Location.randomCity(randomForDemographics)

      val out = new util.HashMap[String, AnyRef]
      val race = city.pickRace(randomForDemographics)
      out.put(Person.RACE, race)

      val ethnicity = city.ethnicityFromRace(race, randomForDemographics)
      out.put(Person.ETHNICITY, ethnicity)

      val language = city.languageFromEthnicity(ethnicity, randomForDemographics)
      out.put(Person.FIRST_LANGUAGE, language)

      val gender = city.pickGender(randomForDemographics)

      if (gender.equalsIgnoreCase("male") || gender.equalsIgnoreCase("M")) gender = "M"
      else gender = "F"

      out.put(Person.GENDER, gender)

      clinician.attributes.putAll(out)

      var firstName = LifecycleModule.fakeFirstName(gender, language, clinician.random)
      var lastName = LifecycleModule.fakeLastName(language, clinician.random)
      if (LifecycleModule.appendNumbersToNames) {
        firstName = LifecycleModule.addHash(firstName)
        lastName = LifecycleModule.addHash(lastName)
      }


      Clinician.StateData(
        name = Person.Name(
          firstName = LifecycleModule.fakeFirstName(gender, language, randomForDemographics),
          lastName = LifecycleModule.fakeLastName(language, randomForDemographics),
          maidenName = None,
          namePrefix = Some("Dr."),
          None
        ),
        address = Clinician.ClinicianAddress(),
        education = Clinician.ClinicianEducation()
      )
    }

  }



  /**
    * Randomly chooses a clinician out of a given clinician list.
    *
    * @param specialty - the specialty to choose from
    * @param random    - random to help choose clinician
    * @return A clinician with the required specialty.
    */
  def chooseClinicianList(specialty: String, random: Random): agents.Clinician = {
    val clinicians = this.clinicianMap.get(specialty)
    val doc = clinicians.get(random.nextInt(clinicians.size))
    doc.incrementEncounters
    doc
  }

}


object Provider {

  type ProviderType = String

  object ProviderType {

    val allowed = Set(
        "WELLNESS",
        "AMBULATORY",
        "INPATIENT",
        "EMERGENCY",
        "URGENTCARE"
      )

    def apply(s: String): ProviderType =
      if (allowed.contains(s)) {
        s
      } else {
        throw new IllegalArgumentException(s"Unallowed ProviderType $s")
      }

  }

  val ENCOUNTERS = "encounters"
  val PROCEDURES = "procedures"
  val LABS = "labs"
  val PRESCRIPTIONS = "prescriptions"
  // ArrayList of all providers imported
  private val providerList = new util.ArrayList[Provider]
  private var providerMap = generateQuadTree
  private val statesLoaded = new util.HashSet[String]
  private var loaded = 0
  private val MAX_PROVIDER_SEARCH_DISTANCE = Config.get("generate.maximum_provider_search_distance", "500").toDouble

  /**
    * Find specific service closest to the person, with a maximum distance of 500 kilometers.
    *
    * @param person  The patient who requires the service.
    * @param service The service required. For example, Provider.AMBULATORY.
    * @param time    The date/time within the simulated world, in milliseconds.
    * @return Service provider or null if none is available.
    */
  def findClosestService(person: Person, service: ProviderType, time: Long): Provider = {
    val maxDistance = MAX_PROVIDER_SEARCH_DISTANCE
    var distance = 100
    val step = 100
    var provider = null
    while ( {
      distance <= maxDistance
    }) {
      provider = findService(person, service, distance, time)
      if (provider != null) return provider
      distance += step
    }
    null
  }

  /**
    * Find a service around a given point.
    *
    * @param person         The patient who requires the service.
    * @param service        e.g. Provider.AMBULATORY
    * @param searchDistance in kilometers
    * @param time           The date/time within the simulated world, in milliseconds.
    * @return Service provider or null if none is available.
    */
  private def findService(person: Person, service: String, searchDistance: Double, time: Long) = {
    val coord = person.getLatLon
    val results = providerMap.queryByPointRadius(coord, searchDistance)
    var closest = null
    var provider = null
    var minDistance = Double.MAX_VALUE
    var distance = .0
    import scala.collection.JavaConversions._
    for (item <- results) {
      provider = item.asInstanceOf[Provider]
      if (provider.accepts(person, time) && (provider.hasService(service) || service == null)) {
        distance = item.getLatLon.distance(coord)
        if (distance < minDistance) {
          closest = item.asInstanceOf[Provider]
          minDistance = distance
        }
      }
    }
    closest
  }

  /**
    * Clear the list of loaded and cached providers.
    */
  def clear(): Unit = {
    providerList.clear()
    statesLoaded.clear()
    providerMap = generateQuadTree
    loaded = 0
  }

  /**
    * Generate a quad tree with sufficient capacity and depth to load
    * the biggest states.
    *
    * @return QuadTree.
    */
  private def generateQuadTree = new QuadTree(7500, 25) // capacity, depth


  // states
  sealed trait State
  case object Open extends State
  case object Hiring extends State
  case object Firing extends State
  case object Closed extends State

  sealed trait StateData

  case object Uninitialized extends StateData

  case class Data(random: Random,
                  encounters: Encounters,
                  servicesProvided: List[String],
                  utilization: Map[String, Double],
                  clinicianMap: Map[ClinicianSpecialty, List[ActorRef]],
                  uuid: String,
                  id: String,
                  name: String,
                  location: Location,
                  phone: String,
                  providerType: ProviderType,
                  ownership: String,
                  quality: Int
                 ) extends StateData

}

