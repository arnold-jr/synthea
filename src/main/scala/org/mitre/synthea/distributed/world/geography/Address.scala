package org.mitre.synthea.distributed.world.geography

trait Address {

  val address: String
  val city: String
  val state: String
  val zip: String
}
