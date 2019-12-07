package pl.edu.agh.beexplore.algorithm

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.Grid
import pl.edu.agh.xinuk.simulation.Metrics

import scala.collection.immutable.TreeSet

class BeexploreMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: BeexploreConfig) extends MovesController {

//  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, Metrics) = {
    val grid = Grid.empty(bufferZone)
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
    } {}
    val metrics = BeexploreMetrics()
    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, Metrics) = {
    (grid, BeexploreMetrics())
  }
}
