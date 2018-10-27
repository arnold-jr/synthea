package org.mitre.synthea.distributed.modules

import java.util
import java.util.concurrent.TimeUnit

import org.mitre.synthea.distributed.world.agents
import org.mitre.synthea.engine.Module
import org.mitre.synthea.helpers.Utilities
import org.mitre.synthea.modules.{CardiovascularDiseaseModule, Immunizations}
import org.mitre.synthea.distributed.world.agents.Provider
import org.mitre.synthea.world.concepts.HealthRecord.EncounterType
import org.mitre.synthea.world.concepts.{ClinicianSpecialty, HealthRecord}

final class EncounterModule() extends Module {

  val name = "Encounter"

  override def process(person0: agents.Person, time: Long): Boolean = {

    var person = person0
    var startedEncounter: Boolean = false
    val year: Int = Utilities.getYear(time)

    // add a wellness encounter if this is the right time
    if (person.record.timeSinceLastWellnessEncounter(time) >= recommendedTimeBetweenWellnessVisits(person, time)) {
      val encounter: HealthRecord#Encounter = person.record.encounterStart(time, EncounterType.WELLNESS.toString)
      encounter.name = "Encounter Module Scheduled Wellness"
      encounter.codes.add(EncounterModule.ENCOUNTER_CHECKUP)

      val prov: Provider = person.getProvider(Provider.AMBULATORY, time)
      prov.incrementEncounters(EncounterType.WELLNESS.toString, year)

      encounter.provider = prov
      encounter.clinician = prov.chooseClinicianList(ClinicianSpecialty.GENERAL_PRACTICE, person.random)
      encounter.codes.add(EncounterModule.getWellnessVisitCode(person, time))
      person.attributes.put(EncounterModule.ACTIVE_WELLNESS_ENCOUNTER, true)
      startedEncounter = true

    } else if (person.symptomTotal > EncounterModule.EMERGENCY_SYMPTOM_THRESHOLD) {

      if (!person.attributes.containsKey(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL)) person.attributes.put(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL, 0)
      if (person.symptomTotal != person.attributes.get(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL).asInstanceOf[Int]) {
        person.attributes.put(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL, person.symptomTotal)
        person.addressLargestSymptom()
        val encounter: HealthRecord#Encounter = person.record.encounterStart(time, EncounterType.EMERGENCY.toString)
        encounter.name = "Encounter Module Symptom Driven"
        val prov: Provider = person.getEmergencyProvider(time)
        prov.incrementEncounters(EncounterType.EMERGENCY.toString, year)
        encounter.provider = prov
        encounter.clinician = prov.chooseClinicianList(ClinicianSpecialty.GENERAL_PRACTICE, person.random)
        encounter.codes.add(EncounterModule.ENCOUNTER_EMERGENCY)
        person.attributes.put(EncounterModule.ACTIVE_EMERGENCY_ENCOUNTER, true)
        startedEncounter = true
      }

    } else if (person.symptomTotal > EncounterModule.URGENT_CARE_SYMPTOM_THRESHOLD) {

      if (!person.attributes.containsKey(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL)) person.attributes.put(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL, 0)
      if (person.symptomTotal != person.attributes.get(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL).asInstanceOf[Int]) {
        person.attributes.put(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL, person.symptomTotal)
        person.addressLargestSymptom()
        val encounter: HealthRecord#Encounter = person.record.encounterStart(time, EncounterType.URGENTCARE.toString)
        encounter.name = "Encounter Module Symptom Driven"
        val prov: Provider = person.getUrgentCareProvider(time)
        prov.incrementEncounters(EncounterType.URGENTCARE.toString, year)
        encounter.provider = prov
        encounter.clinician = prov.chooseClinicianList(ClinicianSpecialty.GENERAL_PRACTICE, person.random)
        encounter.codes.add(EncounterModule.ENCOUNTER_URGENTCARE)
        person.attributes.put(EncounterModule.ACTIVE_URGENT_CARE_ENCOUNTER, true)
        startedEncounter = true
      }

    } else if (person.symptomTotal > EncounterModule.PCP_SYMPTOM_THRESHOLD) {

      if (!person.attributes.containsKey(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL)) {
        person.attributes.put(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL, 0)
      }

      if (person.symptomTotal != person.attributes.get(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL).asInstanceOf[Int]) {

        person.attributes.put(EncounterModule.LAST_VISIT_SYMPTOM_TOTAL, person.symptomTotal)
        person.addressLargestSymptom()
        val encounter: HealthRecord#Encounter = person.record.encounterStart(time, EncounterType.WELLNESS.toString)
        encounter.name = "Encounter Module Symptom Driven"
        val prov: Provider = person.getAmbulatoryProvider(time)
        prov.incrementEncounters(EncounterType.WELLNESS.toString, year)
        encounter.provider = prov
        encounter.clinician = prov.chooseClinicianList(ClinicianSpecialty.GENERAL_PRACTICE, person.random)
        encounter.codes.add(EncounterModule.ENCOUNTER_CHECKUP)
        person.attributes.put(EncounterModule.ACTIVE_WELLNESS_ENCOUNTER, true)
        startedEncounter = true
      }
    }

    if (startedEncounter) {
      CardiovascularDiseaseModule.performEncounter(person, time)
      Immunizations.performEncounter(person, time)
    }
    // java modules will never "finish"
    false
  }

  def recommendedTimeBetweenWellnessVisits(person: agents.Person, time: Long): Long = {
    val ageInYears: Int = person.ageInYears(time)
    if (ageInYears <= 3) {
      val ageInMonths: Int = person.ageInMonths(time)
      if (ageInMonths <= 1) Utilities.convertTime("months", 1)
      else if (ageInMonths <= 5) Utilities.convertTime("months", 2)
      else if (ageInMonths <= 17) Utilities.convertTime("months", 3)
      else Utilities.convertTime("months", 6)
    }
    else if (ageInYears <= 19) Utilities.convertTime("years", 1)
    else if (ageInYears <= 39) Utilities.convertTime("years", 3)
    else if (ageInYears <= 49) Utilities.convertTime("years", 2)
    else Utilities.convertTime("years", 1)
  }

  def endWellnessEncounter(person: agents.Person, time: Long): Unit = {
    person.record.encounterEnd(time, EncounterType.WELLNESS.toString)
    person.attributes.remove(EncounterModule.ACTIVE_WELLNESS_ENCOUNTER)
  }

  def endUrgentCareEncounter(person: agents.Person, time: Long): Unit = {
    person.record.encounterEnd(time, EncounterType.URGENTCARE.toString)
    person.attributes.remove(EncounterModule.ACTIVE_URGENT_CARE_ENCOUNTER)
  }
}

object EncounterModule {

  val ACTIVE_WELLNESS_ENCOUNTER: String = "active_wellness_encounter"
  val ACTIVE_URGENT_CARE_ENCOUNTER: String = "active_urgent_care_encounter"
  val ACTIVE_EMERGENCY_ENCOUNTER: String = "active_emergency_encounter"
  /**
    * These are thresholds for patients to seek symptom-driven care - they'll go to
    * the appropriate provider based on which threshold they meet.
    * By CDC statistics (https://www.cdc.gov/nchs/data/ahcd/namcs_summary/2015_namcs_web_tables.pdf),
    * a person goes to an average of
    * 24,904,00/(US adult population = 249485228) = .0998 urgent visits per year.
    * The goal for the number of symptom-driven encounters (urgent care, PCP, and ER) is .0998 * age.
    */
  val PCP_SYMPTOM_THRESHOLD: Int = 300
  val URGENT_CARE_SYMPTOM_THRESHOLD: Int = 350
  val EMERGENCY_SYMPTOM_THRESHOLD: Int = 500
  val LAST_VISIT_SYMPTOM_TOTAL: String = "last_visit_symptom_total"
  val ENCOUNTER_CHECKUP: HealthRecord.Code = new HealthRecord.Code("SNOMED-CT", "185349003", "Encounter for check up (procedure)")
  val ENCOUNTER_EMERGENCY: HealthRecord.Code = new HealthRecord.Code("SNOMED-CT", "50849002", "Emergency Encounter")
  val WELL_CHILD_VISIT: HealthRecord.Code = new HealthRecord.Code("SNOMED-CT", "410620009", "Well child visit (procedure)")
  val GENERAL_EXAM: HealthRecord.Code = new HealthRecord.Code("SNOMED-CT", "162673000", "General examination of patient (procedure)")
  val ENCOUNTER_URGENTCARE: HealthRecord.Code = new HealthRecord.Code("SNOMED-CT", "371883000", "Outpatient procedure (procedure)")

  // NOTE: if new codes are added, be sure to update getAllCodes below
  def getWellnessVisitCode(person: agents.Person, time: Long): HealthRecord.Code = {
    val age: Int = person.ageInYears(time)
    if (age < 18) WELL_CHILD_VISIT
    else GENERAL_EXAM
  }

  def emergencyVisit(person: agents.Person, time: Long): Unit = { // processes all emergency events. Implemented as a function instead of a rule because
    // emergency events must be processed immediately rather than waiting til the next time
    // period. Patient may die, resulting in rule not being called.
    import scala.collection.JavaConversions._
    for (event <- person.events.before(time, "emergency_encounter")) {
      if (event.processed) continue //todo: continue is not supported
      event.processed = true
      emergencyEncounter(person, time)
    }
    import scala.collection.JavaConversions._
    for (event <- person.events.before(time)) {
      if (event.processed || !event.`type` == "myocardial_infarction" || event.`type` == "cardiac_arrest" || event.`type` == "stroke") continue //todo: continue is not supported
      event.processed = true
      CardiovascularDiseaseModule.performEmergency(person, time, event.`type`)
    }
  }

  def emergencyEncounter(person: agents.Person, time: Long): Unit = { // find closest service provider with emergency service
    val provider: Provider = person.getEmergencyProvider(time)
    provider.incrementEncounters("emergency", Utilities.getYear(time))
    val encounter: HealthRecord#Encounter = person.record.encounterStart(time, "emergency")
    encounter.codes.add(ENCOUNTER_EMERGENCY)
    // TODO: emergency encounters need their duration to be defined by the activities performed
    // based on the emergencies given here (heart attack, stroke)
    // assume people will be in the hospital for observation for a few days
    person.record.encounterEnd(time + TimeUnit.DAYS.toMillis(4), "emergency")
  }

  def urgentCareEncounter(person: agents.Person, time: Long): Unit = { // find closest service provider with urgent care service
    val provider: Provider = person.getUrgentCareProvider(time)
    provider.incrementEncounters("urgent_care", Utilities.getYear(time))
    val encounter: HealthRecord#Encounter = person.record.encounterStart(time, "urgent_care")
    encounter.codes.add(ENCOUNTER_URGENTCARE)
    // assume people will be in urgent care for one hour
    person.record.encounterEnd(time + TimeUnit.HOURS.toMillis(1), "urgent_care")
  }

  /**
    * Get all of the Codes this module uses, for inventory purposes.
    *
    * @return Collection of all codes and concepts this module uses
    */
  def getAllCodes: util.Collection[HealthRecord.Code] = util.Arrays.asList(ENCOUNTER_CHECKUP, ENCOUNTER_EMERGENCY, WELL_CHILD_VISIT, GENERAL_EXAM, ENCOUNTER_URGENTCARE)
}

