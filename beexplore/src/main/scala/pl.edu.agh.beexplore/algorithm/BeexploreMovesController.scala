package pl.edu.agh.beexplore.algorithm

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.accesibles.BeeAccessible
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._
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

  private def calculatePossibleDestinations(cell: Bee, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map { case (i, j) => cell.smell(i)(j) }
      .zipWithIndex
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map { case (_, idx) =>
        val (i, j) = neighbourCellCoordinates(idx)
        (i, j, grid.cells(i)(j))
      }
  }

  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }
      .collectFirstOpt {
        case (i: Int, j: Int, currentCell, BeeAccessible(_)) =>
          (i, j, currentCell)
      }
  }

  private def makeMove(grid: Grid, newGrid: Grid, x: Int, y: Int): Unit = {
    grid.cells(x)(y) match {
      case bee: Bee =>
        val possibleDestinations = calculatePossibleDestinations(bee, x, y, grid)
        selectDestinationCell(possibleDestinations, newGrid)
          .forEmpty {
            newGrid.cells(x)(y) = bee.copy(hunger = bee.hunger + 1)
          }
          .foreach { case (newX, newY, cell) =>
            cell match {
              case BeeAccessible(dest) =>
                newGrid.cells(newX)(newY) = dest.withBee(bee.experience + 1, bee.hunger + 1, bee.role)
                val vacated = EmptyCell(cell.smell)
                newGrid.cells(x)(y) = vacated
                grid.cells(x)(y) = vacated
            }
          }
      case _ => ()
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
