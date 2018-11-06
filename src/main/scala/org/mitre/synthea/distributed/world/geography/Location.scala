package org.mitre.synthea.distributed.world.geography

import com.google.common.collect.Table
import org.mitre.synthea.helpers.{Config, SimpleCSV, Utilities}
import org.mitre.synthea.world.geography.Demographics


class Location(val state: String, val city: String) {

  val allDemographics = Demographics.load(state)
  // this still works even if only 1 city given,
  // because allDemographics will only contain that 1 city
  val demographics = allDemographics.row(state)
  if (city != null && !demographics.containsKey(city)) throw new Exception("The city " + city + " was not found in the demographics file.")

  import scala.collection.JavaConversions._
  // TODO: ensure consistent iteration order)

  val populationByCity: Map[String, Long] = demographics.values.map { d => d.city -> d.population}.toMap

  val totalPopulation: Long = populationByCity.values.sum

  private val filename = Config.get("generate.geography.zipcodes.default_file")

  val csv = Utilities.readResource(filename)
  import scala.collection.JavaConversions._
  val ziplist = SimpleCSV.parse(csv)

  val zipCodes: Map[String, Vector[Place]] = ziplist.foldLeft(Map.empty[String, Vector[Place]]) { (acc, line) =>
    val place: Place = ???
    if (place.sameState(state)) {
      acc ++ Map(place.name -> acc.getOrElse(place.name, Vector.empty[Place]))
    } else {
      acc
    }
  }
}