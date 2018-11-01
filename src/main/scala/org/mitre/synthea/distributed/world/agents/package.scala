package org.mitre.synthea.distributed.world

import org.mitre.synthea.distributed.world.agents.Provider.ProviderType
import org.mitre.synthea.engine.Logic.VitalSign
import org.mitre.synthea.world.concepts.HealthRecord


package object agents {

  case class PersonName(
                         FIRST_NAME: String,
                         LAST_NAME: String,
                         MAIDEN_NAME: String,
                         NAME_PREFIX: String,
                         NAME_SUFFIX: String,
                         NAME: String
                       )

  case class BirthPlace(
                         address: String,
                         city: String,
                         state: String,
                         zip: String,
                         birthplace: String
                       )

  type PreferredProviders = Map[ProviderType, Provider]
  object PreferredProviders {
    val empty: PreferredProviders = Map.empty[ProviderType, Provider]
  }

  case class VitalSigns(
                        height: Option[Double] = None,
                        weight: Option[Double] = None,
                        heightPercentile: Option[Double] = None,
                        weightPercentile: Option[Double] = None,
                        bmi: Option[Double] = None
                       )

  type SymptomType = String
  type SymptomCause = String
  type Symptoms = Map[SymptomType, Map[SymptomCause, (Boolean, Int)]]
  object Symptoms {
    val empty: Symptoms = Map.empty[SymptomType, Map[SymptomCause, (Boolean, Int)]]
  }


  type Encounters = Map[String, HealthRecord#Encounter]
  object Encounters {
    val empty: Encounters = Map.empty[String, HealthRecord#Encounter]
  }

  type CauseOfDeath = Option[HealthRecord.Code]

  type RACE = String
  type ETHNICITY = String
  type FIRST_LANGUAGE = String
  type GENDER = String
  type MULTIPLE_BIRTH_STATUS = String
  type TELECOM = String
  type ID = String
  type COORDINATE = String
  type NAME_MOTHER = String
  type NAME_FATHER = String
  type MARITAL_STATUS = String
  type SOCIOECONOMIC_SCORE = String
  type SOCIOECONOMIC_CATEGORY = String
  type INCOME = String
  type INCOME_LEVEL = String
  type EDUCATION = String
  type EDUCATION_LEVEL = String
  type OCCUPATION_LEVEL = String
  type SMOKER = String
  type ALCOHOLIC = String
  type ADHERENCE = String
  type IDENTIFIER_SSN = String
  type IDENTIFIER_DRIVERS = String
  type IDENTIFIER_PASSPORT = String
  type CAUSE_OF_DEATH = String
  type SEXUAL_ORIENTATION = String
  type LOCATION = String
  type CURRENT_ENCOUNTERS = String

}
