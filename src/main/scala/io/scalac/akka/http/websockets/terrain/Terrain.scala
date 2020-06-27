package io.scalac.akka.http.websockets.terrain


sealed trait Pos {
  val getCubeCoords: CubeCoords
  val getOffsetCoords: OffsetCoords

  override def equals(o: Any): Boolean = o match {
    case OffsetCoords => super.equals()
    case pos :Pos => (this.getOffsetCoords.x == pos.getOffsetCoords.x && this.getOffsetCoords.y == pos.getOffsetCoords.y)
    case _ => false
  }

  def plusVector(vector: (Int, Int, Int)): Pos =
    this.getCubeCoords  match {
      case cubeCords @ CubeCoords(z1,z2,z3)  => CubeCoords(z1+ vector._1,z2+ vector._2 ,z3 + vector._3)
    }
}

case class CubeCoords(z1: Int, z2: Int, z3: Int) extends Pos {
  override lazy val getCubeCoords: CubeCoords = this

  override lazy val getOffsetCoords: OffsetCoords = {
    val offset = z3 % 2
    val x = z1 + (z3 - offset)/2
    OffsetCoords(x, z3)
  }

}

case class OffsetCoords(x: Int, y: Int) extends Pos {
  override lazy val getCubeCoords: CubeCoords = {
    val offset = y % 2
    val z1 = x - (y - offset)/2
    val z2 = -(z1 + y)
    CubeCoords(z1, z2, y)
  }

  override lazy val getOffsetCoords: OffsetCoords = this
}

trait Terrain {
  // TODO Rename it
  val rows: Int
  val maxColumns: Int

  val neighbourVecs : List[(Int, Int, Int)] = for {
    a <- List(-1, 0, +1)
    b <- List(-1, 0, +1)
    if (a != b)
  } yield (a, b, -(a + b))

  def terrainFunction: Pos => Boolean = pos => pos.getOffsetCoords match{
    case OffsetCoords(row,col) =>
      val offset = col % 2
      row < (rows - offset) && row >= 0 && col >= 0 && col < maxColumns
  }

  def getNeighbours(pos: Pos): List[Pos] = {
    if (!terrainFunction(pos)) Nil
    else neighbourVecs.map(vec => pos.plusVector(vec)).filter(terrainFunction(_))
  }

}
