package org.mitre.synthea.distributed.world.geography


/**
  *
  * @param state The name of the state. For example, Ohio
  * @param abbreviation The state abbreviation. For example, OH
  * @param name The name of the place. For example, Columbus
  * @param postalCode The postal code. For example, 01001
  * @param lat
  * @param lon
  */
case class Place(
                state: String,
                abbreviation: String,
                name: String,
                postalCode: String,
                lat: Option[Double] = None,
                lon: Option[Double] = None
                ) {

  /**
    * Check whether or not this Place is in the given state.
    *
    * @param state Name or Abbreviation
    * @return true if they are the same state, otherwise false.
    */
  def sameState(state: String): Boolean = this.state.equalsIgnoreCase(state) || this.abbreviation.equalsIgnoreCase(state)

}

object Place {

  def apply(row: java.util.Map[String, String]): Place = Place(
    state = row.get("USPS"),
    abbreviation = row.get("ST"),
    name = row.get("NAME"),
    postalCode = row.get("ZCTA5"),
    lat = Some(row.get("LAT").toDouble),
    lon = Some(row.get("LON").toDouble)
  )

}
