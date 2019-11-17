package pl.edu.agh.beexplore.utils

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{BeexploreCell, Id}
import pl.edu.agh.xinuk.model.{Cell, Grid, Obstacle}

class BeeUtils(implicit config: BeexploreConfig) {

  import Cell._
    // [TODO IO] - Przerobić na rekursję ogonową
  def processNeighbourFlowerPatches(x: Int, y: Int, id: Int, grid: Grid, imgFlowerPatch: Array[Array[Boolean]]): Unit = {
    if (imgFlowerPatch(y)(x)) {
      val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
      imgFlowerPatch(y)(x) = false
      grid.cells(x)(y) = BeexploreCell(Cell.emptySignal + config.flowerPatchSignalMultiplier, Vector.empty, Id(id))
      neighbourCellCoordinates
        .map { case (i, j) => (grid.cells(i)(j), i, j) }
        .filter(_._1 != Obstacle)
        .filter(r => imgFlowerPatch(r._3)(r._2))
        .foreach { case (_, i, j) => processNeighbourFlowerPatches(i, j, id, grid, imgFlowerPatch) }
    }
  }
}
