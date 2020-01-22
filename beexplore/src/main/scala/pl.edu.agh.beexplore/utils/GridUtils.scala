package pl.edu.agh.beexplore.utils

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.xinuk.model.{EmptyCell, Grid}

import scala.collection.immutable.TreeSet

object GridUtils {
  def deepCopyPreviousGrid(grid: Grid, bufferZone: TreeSet[(Int, Int)])(implicit config: BeexploreConfig): Grid = {
    val newGrid = Grid.empty(bufferZone)
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if notInBufferZone(x, y)
    } {
      grid.cells(x)(y) match {
        case cell: EmptyCell =>
          newGrid.cells(x)(y) = cell.copy()
        case cell: FlowerPatch =>
          newGrid.cells(x)(y) = cell.copy()
        case cell: Bee =>
          newGrid.cells(x)(y) = cell.copy()
        case cell: Beehive =>
          newGrid.cells(x)(y) = cell.copy()
        case cell =>
          println(cell)
      }
    }
    newGrid
  }

  def notInBufferZone(x: Int, y: Int)(implicit config: BeexploreConfig) = {
    x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
  }
}
