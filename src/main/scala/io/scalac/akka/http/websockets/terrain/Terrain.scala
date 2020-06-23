package io.scalac.akka.http.websockets.terrain


sealed trait Pos {
  val getCubeCoords: CubeCoords
  val getOffsetCoords: OffsetCoords

  override def equals(o: Any): Boolean = o match {
    case OffsetCoords => super.equals()
    case pos :Pos => (this.getOffsetCoords.x == pos.getOffsetCoords.x && this.getOffsetCoords.y == pos.getOffsetCoords.y)
    case _ => false
  }

  def plusVector(vector: (Int, Int, Int)): Option[Pos] =
    this.getCubeCoords  match {
      case CubeCoords(z1,z2,z3)  =>   (CubeCoords _).tupled((z1,z2,z3) |+| vector)
    }
//    val (z1,z2,z3) = this.getCubeCoords


}
case class CubeCoords(z1: Int, z2: Int, z3: Int) extends Pos {
  override lazy val getCubeCoords: CubeCoords = this

  override lazy val getOffsetCoords: OffsetCoords = {
    val offset = z3 % 2
    val y = z1 + (z3 - offset)/2
    OffsetCoords(z3, y)
  }

}

case class OffsetCoords(x: Int, y: Int) extends Pos {
  override lazy val getCubeCoords: CubeCoords = {
    val offset = x % 2
    val z1 = y - (x - offset)/2
    val z2 = -y - (x + offset)/2
    CubeCoords(z1, z2, x)
  }

  override lazy val getOffsetCoords: OffsetCoords = this
}

trait Terrain {
  // TODO Rename it
  val rows: Int
  val maxColumns: Int

  val neighbourVecs = for {
    a <- List(-1, 0, +1)
    b <- List(-1, 0, +1)
    if (a != b)
  } yield (a, b, a - b)

  def terrainFunction: Pos => Boolean = pos => pos.getOffsetCoords match{
    case OffsetCoords(row,col) =>
      val offset = col % 2
      row < (rows - offset) && row >= 0 && col >= 0 && col < maxColumns
  }

  def getNeighbours(pos: Pos): List[Pos] = {
    for {
      neighbourVec <- neighbourVecs

    }
  }

}
