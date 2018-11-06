package org.mitre.synthea.distributed.world

import org.mitre.synthea.distributed.{HasLimitedRange, HasLimitedScope}
import org.mitre.synthea.distributed.world.agents.Provider.ProviderType
import org.mitre.synthea.world.concepts.HealthRecord


package object agents {


  type PreferredProviders = Map[ProviderType, Provider]
  object PreferredProviders {
    val empty: PreferredProviders = Map.empty[ProviderType, Provider]
  }


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

  type Language = String
  object Language extends HasLimitedScope[Language, String] {
    override val allowed: Set[String] = Set("english", "spanish")
  }

  type Gender = String
  object Gender extends HasLimitedScope[Gender, String] {
    override val allowed: Set[String] = Set("F", "M")
  }

  type MULTIPLE_BIRTH_STATUS = String
  type TELECOM = String
  type ID = String
  type COORDINATE = String
  type NAME_MOTHER = String
  type NAME_FATHER = String

  type MaritalStatus = String
  object MaritalStatus extends HasLimitedScope [MaritalStatus, String] {
    override val allowed: Set[String] = Set("S", "D", "M")
  }

  type SOCIOECONOMIC_SCORE = String
  type SOCIOECONOMIC_CATEGORY = String
  type INCOME = String
  type INCOME_LEVEL = String
  type EDUCATION = String
  type EducationLevel = Double
  object EducationLevel extends HasLimitedRange[EducationLevel, Double] {
    override val start: EducationLevel = 0.0
    override val stop: EducationLevel = 1.0
  }

  type OCCUPATION_LEVEL = String
  type SMOKER = String
  type ALCOHOLIC = String
  type ADHERENCE = String
  type IDENTIFIER_SSN = String
  type IDENTIFIER_DRIVERS = String
  type IDENTIFIER_PASSPORT = String
  type CAUSE_OF_DEATH = String

  type SexualOrientation = String
  object SexualOrientation extends HasLimitedScope[SexualOrientation, String] {
    override val allowed: Set[String] = Set("lesbian", "gay", "bisexual", "trans", "straight")
  }

  type LOCATION = String
  type CURRENT_ENCOUNTERS = String



}
