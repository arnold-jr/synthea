package org.mitre.synthea.distributed.world

import org.mitre.synthea.distributed.HasLimitedScope

package object concepts {

  type ClinicianSpecialty = String

  object ClinicianSpecialty extends HasLimitedScope[ClinicianSpecialty, String] {

    override val allowed: Set[String] = Set(
      "ADDICTION_MEDICINE",
      "ADVANCED_HEART_FAILURE_AND_TRANSPLANT_CARDIOLOGY",
      "ALLERGY_IMMUNOLOGY",
      "ANESTHESIOLOGY",
      "ANESTHESIOLOGY_ASSISTANT",
      "AUDIOLOGIST",
      "CARDIAC_ELECTROPHYSIOLOGY",
      "CARDIAC_SURGERY",
      "CARDIOLOGY",
      "CERTIFIED_NURSE_MIDWIFE",
      "CERTIFIED_REGISTERED_NURSE_ANESTHETIST",
      "CHIROPRACTIC",
      "CLINICAL_NURSE_SPECIALIST",
      "CLINICAL_PSYCHOLOGIST",
      "CLINICAL_SOCIAL_WORKER",
      "PROCTOLOGY",
      "INTENSIVIST",
      "DENTIST",
      "DERMATOLOGY",
      "DIAGNOSTIC_RADIOLOGY",
      "EMERGENCY_MEDICINE",
      "ENDOCRINOLOGY",
      "FAMILY_PRACTICE",
      "GASTROENTEROLOGY",
      "GENERAL_PRACTICE",
      "GENERAL_SURGERY",
      "GERIATRIC_MEDICINE",
      "GERIATRIC_PSYCHIATRY",
      "GYNECOLOGICAL_ONCOLOGY",
      "HAND_SURGERY",
      "HEMATOLOGY",
      "HEMATOLOGY_ONCOLOGY",
      "HEMATOPOIETIC_CELL_TRANSPLANTATION_AND_CELLULAR_TH",
      "HOSPICE_AND_PALLIATIVE_CARE",
      "HOSPITALIST",
      "INFECTIOUS_DISEASE",
      "INTERNAL_MEDICINE",
      "INTERVENTIONAL_CARDIOLOGY",
      "INTERVENTIONAL_PAIN_MANAGEMENT",
      "INTERVENTIONAL_RADIOLOGY",
      "MAXILLOFACIAL_SURGERY",
      "MEDICAL_ONCOLOGY",
      "NEPHROLOGY",
      "NEUROLOGY",
      "NEUROPSYCHIATRY",
      "NEUROSURGERY",
      "NUCLEAR_MEDICINE",
      "NURSE_PRACTITIONER",
      "OBSTETRICS_GYNECOLOGY",
      "OCCUPATIONAL_THERAPY",
      "OPHTHALMOLOGY",
      "OPTOMETRY",
      "ORAL_SURGERY",
      "ORTHOPEDIC_SURGERY",
      "OSTEOPATHIC_MANIPULATIVE_MEDICINE",
      "OTOLARYNGOLOGY",
      "PAIN_MANAGEMENT",
      "PATHOLOGY",
      "PEDIATRIC_MEDICINE",
      "PERIPHERAL_VASCULAR_DISEASE",
      "PHYSICAL_MEDICINE_AND_REHABILITATION",
      "PHYSICAL_THERAPY",
      "PHYSICIAN_ASSISTANT",
      "PLASTIC_AND_RECONSTRUCTIVE_SURGERY",
      "PODIATRY",
      "PREVENTATIVE_MEDICINE",
      "PSYCHIATRY",
      "PULMONARY_DISEASE",
      "RADIATION_ONCOLOGY",
      "REGISTERED_DIETITIAN_OR_NUTRITION_PROFESSIONAL",
      "RHEUMATOLOGY",
      "SLEEP_MEDICINE",
      "SPEECH_LANGUAGE_PATHOLOGIST",
      "SPORTS_MEDICINE",
      "SURGICAL_ONCOLOGY",
      "THORACIC_SURGERY",
      "UNDEFINED",
      "UROLOGY",
      "VASCULAR_SURGERY"
    )

    lazy val getSpecialties: List[String] = {
      allowed.toList.map(s => s.replace('_', ' ')).sorted
    }
  }

  trait Education {
    val gradeSchool: Option[Boolean]
    val highSchool: Option[Boolean]
    val bachelors: Option[Boolean]
    val masters: Option[Boolean]

  }
}
