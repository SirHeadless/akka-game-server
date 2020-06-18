package io.scalac.akka.http.websockets.terrain


sealed trait Pos {
  def getCubeCoords: CubeCoords
  def getOffsetCoords: OffsetCoords

  override def equals(o: Any): Boolean = o match {
    case OffsetCoords => super.equals()
    case pos :Pos => (this.getOffsetCoords.x == pos.getOffsetCoords.x && this.getOffsetCoords.y == pos.getOffsetCoords.y)
    case _ => false
  }
}
case class CubeCoords(z1: Int, z2: Int, z3: Int) extends Pos {
  override def getCubeCoords: CubeCoords = this

  override def getOffsetCoords: OffsetCoords = {
    val offset = z3 % 2
    val y = z1 + (z3 - offset)/2
    OffsetCoords(z3, y)
  }

}

case class OffsetCoords(x: Int, y: Int) extends Pos {
  override def getCubeCoords: CubeCoords = {
    val offset = x % 2
    val z1 = y - (x - offset)/2
    val z2 = -y - (x + offset)/2
    CubeCoords(z1, z2, x)
  }

  override def getOffsetCoords: OffsetCoords = this
}

trait Terrain {
  // TODO Rename it
  val rows: Int
  val maxColumns: Int

  def terrainFunction: Pos => Boolean = pos => pos.getOffsetCoords match{
    case OffsetCoords(row,col) =>
      val offset = col % 2
      row < (rows - offset) && row >= 0 && col >= 0 && col < maxColumns
  }

}
