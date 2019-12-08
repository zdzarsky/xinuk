package pl.edu.agh.beexplore.algorithm

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{Grid, Signal}
import pl.edu.agh.xinuk.simulation.Metrics

import scala.collection.immutable.TreeSet

class BeexploreMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: BeexploreConfig) extends MovesController {

  //  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, Metrics) = {
    val grid = Grid.empty(bufferZone)
    createBeehive(grid)
    createFlowerPatches(grid)
    spawnBee(grid)
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if notInBufferZone(x, y)
    } {

    }

    (grid, BeexploreMetrics())
  }


  override def makeMoves(iteration: Long, grid: Grid): (Grid, Metrics) = {
    val newGrid = grid
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if notInBufferZone(x, y)
    } {
      makeMove(grid, newGrid, x, y)
    }
    (newGrid, BeexploreMetrics())
  }

  private def makeMove(grid: Grid, newGrid: Grid, x: Int, y: Int) = {

    grid.cells(x)(y) match {
      case Bee(_, _, _, _) =>
      case _ =>
    }
  }

  private def createFlowerPatches(grid: Grid): Unit = {
    for {x <- 50 to 60; y <- 50 to 60} {
      grid.cells(x)(y) = FlowerPatch.create(initialSignal = Signal.apply(3))
    }
  }

  private def spawnBee(grid: Grid): Unit = {
    grid.cells(20)(20) = Bee.create(config.beeSignalInitial)
  }

  private def createBeehive(grid: Grid): Unit = {
    grid.cells(config.beehiveX)(config.beehiveY) = Beehive.create(Signal.Zero)
  }

  private def notInBufferZone(x: Int, y: Int) = {
    x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
  }
}

private object BeexploreMovesController {

}
