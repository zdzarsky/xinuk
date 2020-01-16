package pl.edu.agh.beexplore.algorithm

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.accesibles.{BeeAccessible, FlowerPatchAccesible}
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.simulation.Metrics

import scala.collection.immutable.TreeSet

class BeexploreMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: BeexploreConfig) extends MovesController {

  private val hivePosition: (Int, Int) = (config.beehiveX, config.beehiveY)
  private val hive = Beehive.create(Signal.Zero, hivePosition)

  override def initialGrid: (Grid, Metrics) = {

    val grid = Grid.empty(bufferZone)
    createBeehive(grid)
    createFlowerPatches(grid)
    spawnBee(grid)
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if notInBufferZone(x, y)
    } {}
    (grid, BeexploreMetrics())
  }


  def deepCopyPreviousGrid(grid: Grid): Grid = {
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

  override def makeMoves(iteration: Long, grid: Grid): (Grid, Metrics) = {
    val newGrid = deepCopyPreviousGrid(grid)
    feedBeesAndReleaseScouts(grid, newGrid)
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if notInBufferZone(x, y)
    } {
      makeMove(newGrid, grid, x, y)
    }
    (newGrid, BeexploreMetrics())
  }

  private def feedBeesAndReleaseScouts(grid: Grid, newGrid: Grid): Unit = {
    val (stillHungry, notHungry) = grid.cells(hivePosition._1)(hivePosition._2).asInstanceOf[Beehive].bees.partition(_.hunger >= 0)
    newGrid.cells(hivePosition._1)(hivePosition._2) = hive.copy(bees = stillHungry.map(b => b.copy(hunger = b.hunger - 1)))
    notHungry.foreach(bee => newGrid.cells(hivePosition._1 + 3)(hivePosition._2 + 3) = bee)
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

  def selectDestinationCell(bee: Bee, possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): Opt[(Int, Int, GridPart)] = {
    if (bee.hunger > config.beeHungerThreshold) {
      Opt(possibleDestinations.reduceLeft((p1, p2) => if (distanceFromHive(p1._1, p1._2) < distanceFromHive(p2._1, p2._2)) p1 else p2))
    } else {
      possibleDestinations
        .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }
        .collectFirstOpt {
          case (i: Int, j: Int, currentCell, BeeAccessible(_)) =>
            (i, j, currentCell)
        }
    }
  }

  private def distanceFromHive(x1: Int, y1: Int) = {
    Math.abs(x1 - hivePosition._1) + Math.abs(y1 - hivePosition._1)
  }

  private def makeMove(newGrid: Grid, grid: Grid, x: Int, y: Int): Unit = {
    grid.cells(x)(y) match {
      case bee: Bee =>
        val possibleDestinations = calculatePossibleDestinations(bee, x, y, grid)
        selectDestinationCell(bee, possibleDestinations, newGrid)
          .forEmpty {
            newGrid.cells(x)(y) = bee.copy(hunger = bee.hunger + 1)
          }.foreach { case (newX, newY, cell) =>
          cell match {
            case BeeAccessible(dest) =>
              newGrid.cells(newX)(newY) = dest.withBee(bee.experience + 1, bee.hunger + 1)
              grid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
            case Beehive(_, _, bees) =>
              grid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(hivePosition._1)(hivePosition._2) = hive.copy(bees = bee +: bees)
          }
        }
      case flowerPatch: FlowerPatch =>
        Grid.neighbourCellCoordinates(x, y)
          .map(coords => grid.cells(coords._1)(coords._2))
          .map {
            case FlowerPatchAccesible(part) => part.withFlowers(flowerPatch.smell)
            case _ =>
          }
      case _ =>
    }
  }

  private def createFlowerPatches(grid: Grid): Unit = {
    for {x <- 50 to 60; y <- 50 to 60} {
      grid.cells(x)(y) = FlowerPatch.create(initialSignal = Signal.apply(0.4))
    }
  }

  private def spawnBee(grid: Grid): Unit = {
    grid.cells(20)(20) = Bee.create(config.beeSignalInitial)
  }

  private def createBeehive(grid: Grid): Unit = {
    grid.cells(config.beehiveX)(config.beehiveY) = hive
  }

  private def notInBufferZone(x: Int, y: Int) = {
    x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
  }
}

private object BeexploreMovesController {

}
