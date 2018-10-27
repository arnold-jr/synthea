package org.mitre.synthea.distributed.world.agents

import com.google.common.collect.HashBasedTable
import com.google.common.collect.Table
import com.google.gson.internal.LinkedTreeMap
import java.io.IOException
import java.util
import java.util.{ArrayList, HashMap, HashSet, List, Map, Random, Set, UUID}
import java.util.concurrent.atomic.AtomicInteger

import org.apache.sis.geometry.DirectPosition2D
import org.apache.sis.index.tree.QuadTree
import org.apache.sis.index.tree.QuadTreeData
import org.mitre.synthea.helpers.Config
import org.mitre.synthea.helpers.SimpleCSV
import org.mitre.synthea.helpers.Utilities
import org.mitre.synthea.modules.LifecycleModule
import org.mitre.synthea.world.agents
import org.mitre.synthea.world.agents.{Clinician, Person, Provider}
import org.mitre.synthea.world.concepts.ClinicianSpecialty
import org.mitre.synthea.world.geography.Demographics
import org.mitre.synthea.world.geography.Location


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

  val WELLNESS = "wellness"
  val AMBULATORY = "ambulatory"
  val INPATIENT = "inpatient"
  val EMERGENCY = "emergency"
  val URGENTCARE = "urgent care"
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

  /**
    * Load into cache the list of providers for a state.
    *
    * @param location the state being loaded.
    */
  def loadProviders(location: Location): Unit = {
    if (!statesLoaded.contains(location.state) || !statesLoaded.contains(Location.getAbbreviation(location.state)) || !statesLoaded.contains(Location.getStateName(location.state))) try {
      val servicesProvided = new util.HashSet[String]
      servicesProvided.add(Provider.AMBULATORY)
      servicesProvided.add(Provider.INPATIENT)
      val hospitalFile = Config.get("generate.providers.hospitals.default_file")
      loadProviders(location, hospitalFile, servicesProvided)
      val vaFile = Config.get("generate.providers.veterans.default_file")
      loadProviders(location, vaFile, servicesProvided)
      servicesProvided.clear()
      servicesProvided.add(Provider.WELLNESS)
      val primaryCareFile = Config.get("generate.providers.primarycare.default_file")
      loadProviders(location, primaryCareFile, servicesProvided)
      servicesProvided.clear()
      servicesProvided.add(Provider.URGENTCARE)
      val urgentcareFile = Config.get("generate.providers.urgentcare.default_file")
      loadProviders(location, urgentcareFile, servicesProvided)
      statesLoaded.add(location.state)
      statesLoaded.add(Location.getAbbreviation(location.state))
      statesLoaded.add(Location.getStateName(location.state))
    } catch {
      case e: IOException =>
        System.err.println("ERROR: unable to load providers for state: " + location.state)
        e.printStackTrace()
    }
  }

  /**
    * Read the providers from the given resource file, only importing the ones for the given state.
    * THIS method is for loading providers and generating clinicians with specific specialties
    *
    * @param location         the state being loaded
    * @param filename         Location of the file, relative to src/main/resources
    * @param servicesProvided Set of services provided by these facilities
    * @throws IOException if the file cannot be read
    */
  @throws[IOException]
  def loadProviders(location: Location, filename: String, servicesProvided: util.Set[String]): Unit = {
    val resource = Utilities.readResource(filename)
    val csv = SimpleCSV.parseLineByLine(resource)
    while ( {
      csv.hasNext
    }) {
      val row = csv.next
      val currState = row.get("state")
      val abbreviation = Location.getAbbreviation(location.state)
      // for now, only allow one state at a time
      if ((location.state == null) || (location.state != null && location.state.equalsIgnoreCase(currState)) || (abbreviation != null && abbreviation.equalsIgnoreCase(currState))) {
        val parsed = csvLineToProvider(row)
        parsed.servicesProvided.addAll(servicesProvided)
        if ("Yes" == row.remove("emergency")) parsed.servicesProvided.add(Provider.EMERGENCY)
        // add any remaining columns we didn't explicitly map to first-class fields
        // into the attributes table
        import scala.collection.JavaConversions._
        for (e <- row.entrySet) {
          parsed.attributes.put(e.getKey, e.getValue)
        }
        parsed.location = location
        // String city = parsed.city;
        // String address = parsed.address;
        if (row.get("hasSpecialties") == null || row.get("hasSpecialties").equalsIgnoreCase("false")) parsed.clinicianMap.put(ClinicianSpecialty.GENERAL_PRACTICE, parsed.generateClinicianList(1, ClinicianSpecialty.GENERAL_PRACTICE))
        else {
          for (specialty <- ClinicianSpecialty.getSpecialties) {
            val specialtyCount = row.get(specialty)
            if (specialtyCount != null && !(specialtyCount.trim == "") && !(specialtyCount.trim == "0")) parsed.clinicianMap.put(specialty, parsed.generateClinicianList(row.get(specialty).toInt, specialty))
          }
          if (row.get(ClinicianSpecialty.GENERAL_PRACTICE) == "0") parsed.clinicianMap.put(ClinicianSpecialty.GENERAL_PRACTICE, parsed.generateClinicianList(1, ClinicianSpecialty.GENERAL_PRACTICE))
        }
        providerList.add(parsed)
        val inserted = providerMap.insert(parsed)
        if (!inserted) throw new RuntimeException("Provider QuadTree Full! Dropping # " + loaded + ": " + parsed.name + " @ " + parsed.city)
        else loaded += 1
      }
    }
  }

  /**
    * Given a line of parsed CSV input, convert the data into a Provider.
    *
    * @param line - read a csv line to a provider's attributes
    * @return A provider.
    */
  private def csvLineToProvider(line: util.Map[String, String]) = {
    val d = new Provider
    d.uuid = UUID.randomUUID.toString
    // using remove instead of get here so that we can iterate over the remaining keys later
    d.id = line.remove("id")
    d.name = line.remove("name")
    d.address = line.remove("address")
    d.city = line.remove("city")
    d.state = line.remove("state")
    d.zip = line.remove("zip")
    d.phone = line.remove("phone")
    d.`type` = line.remove("type")
    d.ownership = line.remove("ownership")
    try
      d.quality = line.remove("quality").toInt
    catch {
      case e: Exception =>

      // Swallow invalid format data
    }
    try {
      val lat = line.remove("LAT").toDouble
      val lon = line.remove("LON").toDouble
      d.coordinates = new DirectPosition2D(lon, lat)
    } catch {
      case e: Exception =>
        val lat = 0.0
        val lon = 0.0
        d.coordinates = new DirectPosition2D(lon, lat)
    }
    d
  }

  def getProviderList: util.List[Provider] = providerList
}

class Provider protected() extends QuadTreeData {
  attributes = new LinkedTreeMap[String, AnyRef]
  utilization = HashBasedTable.create
  servicesProvided = new util.ArrayList[String]
  clinicianMap = new util.HashMap[String, util.ArrayList[agents.Clinician]]
  var attributes: util.Map[String, AnyRef] = null
  var uuid: String = null
  var id: String = null
  var name: String = null
  private var location = null
  var address: String = null
  var city: String = null
  var state: String = null
  var zip: String = null
  var phone: String = null
  var `type`: String = null
  var ownership: String = null
  var quality = 0
  private var coordinates = null
  var servicesProvided: util.ArrayList[String] = null
  var clinicianMap: util.Map[String, util.ArrayList[agents.Clinician]] = null
  // row: year, column: type, value: count
  private var utilization = null

  def getResourceID: String = uuid

  def getAttributes: util.Map[String, AnyRef] = attributes

  def getCoordinates: DirectPosition2D = coordinates

  def hasService(service: String): Boolean = servicesProvided.contains(service)

  def incrementEncounters(encounterType: String, year: Int): Unit = {
    increment(year, Provider.ENCOUNTERS)
    increment(year, Provider.ENCOUNTERS + "-" + encounterType)
  }

  def incrementProcedures(year: Int): Unit = {
    increment(year, Provider.PROCEDURES)
  }

  def incrementLabs(year: Int): Unit = {
    increment(year, Provider.LABS)
  }

  def incrementPrescriptions(year: Int): Unit = {
    increment(year, Provider.PRESCRIPTIONS)
  }

  private def increment(year: Integer, key: String): Unit = {
    if (!utilization.contains(year, key)) utilization.put(year, key, new AtomicInteger(0))
    utilization.get(year, key).incrementAndGet
  }

  def getUtilization: Table[Integer, String, AtomicInteger] = utilization

  /**
    * Get the bed count for this Provider facility.
    *
    * @return The number of beds, if they exist, otherwise null.
    */
  def getBedCount: Integer = if (attributes.containsKey("bed_count")) attributes.get("bed_count").toString.toInt
  else null

  /**
    * Will this provider accept the given person as a patient at the given time?.
    *
    * @param person Person to consider
    * @param time   Time the person seeks care
    * @return whether or not the person can receive care by this provider
    */
  def accepts(person: Person, time: Long): Boolean = { // for now assume every provider accepts every patient
    // UNLESS it's a VA facility and the person is not a veteran
    // eventually we may want to expand this (ex. capacity?)
    if ("VA Facility" == this.`type` && !person.attributes.containsKey("veteran")) return false
    true
  }

  /**
    * Generates a list of clinicians, given the number to generate and the specialty.
    *
    * @param numClinicians - the number of clinicians to generate
    * @param specialty     - which specialty clinicians to generate
    * @return
    */
  private def generateClinicianList(numClinicians: Int, specialty: String) = {
    val clinicians = new util.ArrayList[agents.Clinician]
    var i = 0
    while ( {
      i < numClinicians
    }) {
      var clinician = null
      clinician = generateClinician(i, this)
      clinician.attributes.put(Clinician.SPECIALTY, specialty)
      clinicians.add(clinician)

      {
        i += 1; i - 1
      }
    }
    clinicians
  }

  /**
    * Generate a completely random Clinician.
    * The seed used to generate the person is randomized as well.
    *
    * @param index Target index in the whole set of people to generate
    * @return generated Person
    */
  private def generateClinician(index: Int, provider: Provider) = { // System.currentTimeMillis is not unique enough
    val clinicianSeed = UUID.randomUUID.getMostSignificantBits & Long.MAX_VALUE
    generateClinician(index, clinicianSeed, provider)
  }

  /**
    * Generate a random clinician, from the given seed.
    *
    * @param index
    * Target index in the whole set of people to generate
    * @param clinicianSeed
    * Seed for the random clinician
    * @return generated Clinician
    */
  private def generateClinician(index: Int, clinicianSeed: Long, provider: Provider) = {
    var clinician = null
    try {
      val randomForDemographics = new Random(clinicianSeed)
      val city = location.randomCity(randomForDemographics)
      val out = new util.HashMap[String, AnyRef]
      val race = city.pickRace(randomForDemographics)
      out.put(Person.RACE, race)
      val ethnicity = city.ethnicityFromRace(race, randomForDemographics)
      out.put(Person.ETHNICITY, ethnicity)
      val language = city.languageFromEthnicity(ethnicity, randomForDemographics)
      out.put(Person.FIRST_LANGUAGE, language)
      var gender = city.pickGender(randomForDemographics)
      if (gender.equalsIgnoreCase("male") || gender.equalsIgnoreCase("M")) gender = "M"
      else gender = "F"
      out.put(Person.GENDER, gender)
      clinician = new agents.Clinician(clinicianSeed)
      clinician.attributes.putAll(out)
      clinician.attributes.put(Person.ADDRESS, provider.address)
      clinician.attributes.put(Person.CITY, provider.city)
      clinician.attributes.put(Person.STATE, provider.state)
      clinician.attributes.put(Person.ZIP, provider.zip)
      var firstName = LifecycleModule.fakeFirstName(gender, language, clinician.random)
      var lastName = LifecycleModule.fakeLastName(language, clinician.random)
      if (LifecycleModule.appendNumbersToNames) {
        firstName = LifecycleModule.addHash(firstName)
        lastName = LifecycleModule.addHash(lastName)
      }
      clinician.attributes.put(Clinician.FIRST_NAME, firstName)
      clinician.attributes.put(Clinician.LAST_NAME, lastName)
      clinician.attributes.put(Clinician.NAME, firstName + " " + lastName)
      clinician.attributes.put(Clinician.NAME_PREFIX, "Dr.")
      // Degree's beyond a bachelors degree are not currently tracked.
      clinician.attributes.put(Clinician.EDUCATION, "bs_degree")
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        throw e
    }
    clinician
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

  /*
     * (non-Javadoc)
     * @see org.apache.sis.index.tree.QuadTreeData#getX()
     */ override def getX: Double = coordinates.getX

  /*
     * (non-Javadoc)
     * @see org.apache.sis.index.tree.QuadTreeData#getY()
     */ override def getY: Double = coordinates.getY

  /*
     * (non-Javadoc)
     * @see org.apache.sis.index.tree.QuadTreeData#getLatLon()
     */ override def getLatLon: DirectPosition2D = coordinates

  /*
     * (non-Javadoc)
     * @see org.apache.sis.index.tree.QuadTreeData#getFileName()
     */ override def getFileName: String = null
}

