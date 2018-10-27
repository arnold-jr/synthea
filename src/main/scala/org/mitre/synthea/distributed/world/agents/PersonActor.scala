package org.mitre.synthea.distributed.world.agents

import java.util.Random

import akka.actor.{Actor, ActorLogging}
import org.mitre.synthea.distributed.Protocol
import org.mitre.synthea.distributed.world.agents
import org.mitre.synthea.engine.Generator.GeneratorOptions
import org.mitre.synthea.engine.Module
import org.mitre.synthea.modules.{DeathModule, LifecycleModule}
import org.mitre.synthea.world.geography.{Demographics, Location}

import scala.collection.JavaConverters._
import scala.collection._

class PersonActor(personSeed: Long,
                  timeStep: Long,
                  stop: Long) extends Actor with ActorLogging {

  import Protocol._

  var isAlive = true
  var tryNumber = 0 // number of tries to create these demographics
  val randomForDemographics = new Random(personSeed)
  val city: Demographics = location.randomCity(randomForDemographics)

  val start: Long =
    demoAttributes.getOrElse(Person.BIRTHDATE, throw new Exception("didn't get a person")).asInstanceOf[Long]

  val person = new agents.Person(personSeed)

  person.populationSeed = options.seed
  person.attributes.putAll(demoAttributes.asJava)
  person.attributes.put(Person.LOCATION, location)

  val modules  = Module.getModules

  LifecycleModule.birth(person, start)
  val encounterModule: modules.EncounterModule  = new modules.EncounterModule()

  var time: Long = 0

  override def receive: Receive = {

    case AdvanceOneTimestep => (person.alive(time), time < stop) match {
      case (true, true) =>

        encounterModule.process(person, time)

        val iter = modules.iterator()

        while (iter.hasNext) {
          val module: Module = iter.next()
          log.debug("Processing module {}", module.name)

          if (module.process(person, time)) {
            log.debug("Removing module {}", module.name)
            iter.remove() // this module has completed/terminated.
          }
        }

      case (true, false) =>

      case (false, true) =>

      case (false, false) =>

    }
      DeathModule.process(person, time)

      isAlive = person.alive(time)
      time += timeStep


  }
}
