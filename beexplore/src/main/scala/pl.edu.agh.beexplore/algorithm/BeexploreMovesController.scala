package pl.edu.agh.beexplore.algorithm

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.accesibles.BeeAccessible
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.beexplore.utils.{GridUtils, HoneyWorld, IdealWorld, MapWorld}
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.simulation.Metrics

import scala.collection.immutable.TreeSet

class BeexploreMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: BeexploreConfig) extends MovesController {
   val mapPath = ""
   val world: HoneyWorld = if(mapPath.isEmpty) new IdealWorld() else new MapWorld()

  override def initialGrid: (Grid, Metrics) = {
    val grid = Grid.empty(bufferZone)
    if(mapPath.isEmpty) {
      world.create(grid)
    }
    (grid, BeexploreMetrics())
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, Metrics) = {
    val newGrid = GridUtils.deepCopyPreviousGrid(grid, bufferZone)
    feedBeesAndReleaseScouts(grid, newGrid)
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if(GridUtils.notInBufferZone(x, y))
    } {
      applyBehavior(newGrid, grid, x, y)
    }
    (newGrid, BeexploreMetrics())
  }

  private def feedBeesAndReleaseScouts(grid: Grid, newGrid: Grid): Unit = {
    val hivePosition = world.hive().position
    val (stillHungry, notHungry) = grid.cells(hivePosition._1)(hivePosition._2).asInstanceOf[Beehive].bees.partition(_.hunger >= 0)
    newGrid.cells(hivePosition._1)(hivePosition._2) = world.hive().copy(bees = stillHungry.map(b => b.copy(hunger = b.hunger - 1)))
    notHungry.foreach(bee => newGrid.cells(hivePosition._1 + 3)(hivePosition._2 + 3) = bee) // add here experience based release from hive
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
    val destinations = possibleDestinations.toList.filter(!_._3.isInstanceOf[FlowerPatch])
    if (bee.hunger > config.beeHungerThreshold) {
      Opt(destinations.reduceLeft(
        (p1, p2) =>
          if (distanceFromHive(p1._1, p1._2) < distanceFromHive(p2._1, p2._2)) p1 else p2))
    } else {
      destinations.map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }
        .collectFirstOpt {
          case (i: Int, j: Int, currentCell, BeeAccessible(_)) =>
            (i, j, currentCell)
        }
    }
  }

  private def distanceFromHive(x1: Int, y1: Int) = {
    Math.abs(x1 - world.hive().position._1) + Math.abs(y1 - world.hive().position._1)
  }

  private def applyBehavior(newGrid: Grid, grid: Grid, x: Int, y: Int): Unit = {
    grid.cells(x)(y) match {
      case bee: Bee =>
        Grid.neighbourCellCoordinates(x, y).map(pt => grid.cells(pt._1)(pt._2)).foreach{
          case FlowerPatch(smell) =>
            newGrid.cells(x)(y) = bee.withSmell(bee.smellWithoutArray(smell))
          case _ =>
        }
        val possibleDestinations = calculatePossibleDestinations(bee, x, y, grid)
        selectDestinationCell(bee, possibleDestinations, newGrid)
          .forEmpty {
            newGrid.cells(x)(y) = bee.copy(hunger = bee.hunger + 1)
          }.foreach { case (newX, newY, cell) =>
          cell match {
            case BeeAccessible(dest) =>
              newGrid.cells(newX)(newY) = dest.withBee(bee.smell, bee.experience + 1, bee.hunger + 1)
              grid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
            case Beehive(_, _, bees) =>
              grid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(world.hive().position._1)(world.hive().position._2) = world.hive().copy(bees = bee +: bees)
            case FlowerPatch(smell) =>

          }
        }
      case FlowerPatch(smell) => newGrid.cells(x)(y) = FlowerPatch.create(Signal(0.4))
      case _ =>
    }
  }

}

private object BeexploreMovesController {

}
