package org.mitre.synthea.distributed.world.agents

import java.io.Serializable
import java.util
import java.util.{ArrayList, Map, Random, UUID}
import java.util.concurrent.ConcurrentHashMap

import org.apache.sis.geometry.DirectPosition2D
import org.apache.sis.index.tree.QuadTreeData


@SerialVersionUID(1370111157423846567L)
object Clinician {
  type WELLNESS = String
  type AMBULATORY = String
  type INPATIENT = String
  type EMERGENCY = String
  type URGENTCARE = String
  type FIRST_NAME = String
  type LAST_NAME = String
  type NAME_PREFIX = String
  type NAME_SUFFIX = String
  type NAME = String
  type FIRST_LANGUAGE = String
  type GENDER = String
  type EDUCATION = String
  type SPECIALTY = String
  type ADDRESS = String
  type CITY = String
  type STATE = String
  type ZIP = String
  type LOCATION = String
}

@SerialVersionUID(1370111157423846567L)
class Clinician(val seed: Long) // keep track of seed so it can be exported later

  extends Serializable with QuadTreeData {
  this.uuid = UUID.randomUUID.toString
  random = new Random(seed)
  attributes = new ConcurrentHashMap[String, AnyRef]
  servicesProvided = new util.ArrayList[String]
  final var random: Random = null
  final var uuid: String = null
  var attributes: util.Map[String, AnyRef] = null
  private var servicesProvided = null
  private var encounters = 0
  var populationSeed = 0L

  def getResourceID: String = uuid

  def rand: Double = random.nextDouble

  def getAttributes: util.Map[String, AnyRef] = attributes

  def hasService(service: String): Boolean = servicesProvided.contains(service)

  /**
    * Increment the number of encounters performed by this Clinician.
    *
    * @return The incremented number of encounters.
    */
  def incrementEncounters: Int = {
    encounters += 1; encounters - 1
  }

  /**
    * Get the number of encounters performed by this Clinician.
    *
    * @return The number of encounters.
    */
  def getEncounterCount: Int = encounters

  def randInt: Int = random.nextInt

  def randInt(bound: Int): Int = random.nextInt(bound)

  override def getX: Double = { // TODO Auto-generated method stub
    0
  }

  override def getY = 0

  override def getLatLon: DirectPosition2D = null

  override def getFileName: String = null
}


