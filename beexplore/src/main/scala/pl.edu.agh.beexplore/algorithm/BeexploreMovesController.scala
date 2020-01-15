package pl.edu.agh.beexplore.algorithm

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.collection.CollectionAliases.MMap
import com.avsystem.commons.misc.Opt
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.Bee.{Experience, Experienced, Expert, Intermediate, Novice}
import pl.edu.agh.beexplore.model.accesibles.BeeAccessible
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.simulation.Metrics

import scala.collection.immutable.TreeSet

class BeexploreMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: BeexploreConfig) extends MovesController {

  private val hivePosition: (Int, Int) = (config.beehiveX, config.beehiveY)
  private val hive = Beehive.create(Signal.Zero, hivePosition)

  private val beesPositions: MMap[Int, List[(Int, Int)]] = MMap.empty
  private val partialDistances: MMap[Int, Double] = MMap.empty
  private val perExperienceFlightDistance: MMap[Experience, List[Double]] = MMap(
    Novice -> List.empty,
    Intermediate -> List.empty,
    Experienced -> List.empty,
    Expert -> List.empty
  )
  private val perExperienceConvexHull: MMap[Experience, List[Double]] = MMap(
    Novice -> List.empty,
    Intermediate -> List.empty,
    Experienced -> List.empty,
    Expert -> List.empty
  )

  private var id = 0

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
    (grid, BeexploreMetrics.empty)
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
    (newGrid, BeexploreMetrics(0, 0))
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

  private def calculateDistance(first: (Int, Int), second: (Int, Int)): Double = {
    def pow2(v: Int) = v * v

    Math.sqrt(pow2(first._1 - second._1) + pow2(first._2 - second._2))
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
              newGrid.cells(newX)(newY) = dest.withBee(bee.id,bee.numberOfFlights, bee.experience, bee.hunger + 1, bee.role)
              val distance = calculateDistance(beesPositions(bee.id).lastOpt.getOrElse((20,20)), (newX, newY))
              partialDistances(bee.id) += distance
              beesPositions(bee.id) +:= (newX, newY)
              grid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
            case Beehive(_, _, bees) =>
              grid.cells(x)(y) = EmptyCell(cell.smell)
              newGrid.cells(x)(y) = EmptyCell(cell.smell)
              val newBee = bee.copy(experience = calculateExperience(bee), numberOfFlights = bee.numberOfFlights + 1)
              newGrid.cells(hivePosition._1)(hivePosition._2) = hive.copy(bees = newBee +: bees)
              perExperienceFlightDistance(bee.experience) +:= partialDistances(bee.id)
              partialDistances(bee.id) = 0
              perExperienceConvexHull(bee.experience) +:= calculateConvexHull(bee)
              beesPositions(bee.id) = List()
          }
        }
      case _ =>
    }
  }

  def calculateExperience(bee: Bee): Experience = if (bee.numberOfFlights == 0) Novice
  else if (bee.numberOfFlights == 3) Experienced else if (bee.numberOfFlights == 5) Expert else bee.experience


  private def calculateConvexHull(bee: Bee): Double = {
    var results: Seq[(Int, Int)] = Seq.empty

    def findSide(point: (Int, Int), first: (Int, Int), second: (Int, Int)) =
      math.signum((point._2 - first._2) * (second._1 - first._1) -
        (second._2 - first._2) * (point._1 - first._1))

    def lineDist(point: (Int, Int), first: (Int, Int), second: (Int, Int)) =
      math.abs((point._2 - first._2) * (second._1 - first._1) -
        (second._1 - first._1) * (point._1 - first._1))

    def calculate(positions: Seq[(Int, Int)], first: (Int, Int), second: (Int, Int), side: Int): Unit = {
      val (newPos, maxDist) = positions.zipWithIndex.foldLeft(((Int.MinValue, Int.MinValue), Int.MinValue)) {
        case ((positionAccumulated, maxDist), (pos, i)) => {
          val dist = lineDist(first, second, pos)
          if (findSide(first, second, pos) == side && dist > maxDist) {
            (pos, dist)
          } else (positionAccumulated, maxDist)
        }
      }
      // finding the point with maximum distance
      // from L and also on the specified side of L.

      // If no point is found, add the end points
      // of L to the convex hull.
      if (maxDist == Int.MinValue) {
        results +:= first
        results +:= second
        ()
      } else {
        calculate(positions, newPos, first, -findSide(newPos, first, second))
        calculate(positions, newPos, second, -findSide(newPos, second, first))
      }

      // Recur for the two parts divided by a[ind]
    }

    val positions = beesPositions(bee.id)
    val (min, max) = positions.foldLeft((Int.MaxValue, Int.MaxValue), (Int.MinValue, Int.MinValue)) {
      case ((minXPos, maxXPos), (posX, posY)) =>
        val newMin = if (posX < minXPos._1) (posX, posY) else minXPos
        val newMax = if (posX > maxXPos._1) (posX, posY) else maxXPos
        (newMin, newMax)
    }

    // Recursively find convex hull points on
    // one side of line joining a[min_x] and
    // a[max_x]
    calculate(positions, min, max, 1)
    // other side of line joining a[min_x] and
    calculate(positions, min, max, -1)

    val points = (positions.zip(positions.tail)) :+ (positions.last, positions.head)

    points.map { case (a, b) => a._1 * b._2 - a._2 * b._1 }.sum / 2.0
  }

  private def createFlowerPatches(grid: Grid): Unit = {
    for {x <- 50 to 60; y <- 50 to 60} {
      grid.cells(x)(y) = FlowerPatch.create(initialSignal = Signal.apply(0.4))
    }
  }

  private def spawnBee(grid: Grid): Unit = {
    val bee = Bee.create(id, config.beeSignalInitial)
    id += 1
    grid.cells(20)(20) = bee
    beesPositions(bee.id) = List((20, 20))
    partialDistances(bee.id) = calculateDistance(hivePosition, (20, 20))
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
