package io.scalac.akka.http.websockets.terrain

import org.scalatest.FunSuite

class TerrainSuit extends FunSuite with Terrain {
  override val rows: Int = 10
  override val maxColumns: Int = 7

  test("Expect no neighbours for a invalid position") {
    val test = getNeighbours(OffsetCoords(10,6))
    assert(test == Nil)
  }

  test("Expect two neighbours of field in left down corner") {
    val test = getNeighbours(OffsetCoords(9,6))
    assert( test.contains(OffsetCoords(8,6)) && test.contains(OffsetCoords(8,5)))
  }

}
