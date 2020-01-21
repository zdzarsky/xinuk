package pl.edu.agh.beexplore.algorithm

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.collection.CollectionAliases.MMap
import com.avsystem.commons.misc.Opt
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.Bee._
import pl.edu.agh.beexplore.model.accesibles.BeeAccessible
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.simulation.Metrics

import scala.collection.immutable
import scala.collection.immutable.TreeSet
import scala.util.Random

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

  implicit val ordering: Ordering[(Double, Int, Int, GridPart)] = (x: (Double, Int, Int, GridPart), y: (Double, Int, Int, GridPart)) => math.ceil(x._1 - y._1).toInt

  private def calculatePossibleDestinations(cell: Bee, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map { case (i, j) => cell.smell(i)(j) }
      .map(_.value)
      .zipWithIndex
      .map { case (smell, idx) =>
        val (i, j) = neighbourCellCoordinates(idx)
        (smell, i, j, grid.cells(i)(j))
      }
      .map { case (smell, i, j, cell) =>
        (Random.nextDouble() * calculateDistance((i, j), (30, 30)), i, j, cell)
      }
      .sorted(implicitly[Ordering[(Double, Int, Int, GridPart)]])
      .map { case (_, i, j, cell) => (i, j, cell)
      }
      .iterator
  }

  def getExperienceFactor(bee: Bee) = bee.experience match {
    case Novice => 1
    case Bee.Intermediate => 3
    case Bee.Experienced => 6
    case Bee.Expert => 6
  }

  def selectDestinationCell(bee: Bee, possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): Opt[(Int, Int, GridPart)] = {
    if (bee.hunger > config.beeHungerThreshold * getExperienceFactor(bee)) {
      Opt(possibleDestinations.reduceLeft((p1, p2) => if (distanceFromHive(p1._1, p1._2) <= distanceFromHive(p2._1, p2._2)) p1 else p2))
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
              newGrid.cells(newX)(newY) = dest.withBee(bee.id, bee.numberOfFlights, bee.experience, bee.hunger + 1, bee.role)
              val distance = calculateDistance(beesPositions(bee.id).lastOpt.getOrElse((30, 30)), (newX, newY))
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
              val convexHull = calculateConvexHull(bee)


              perExperienceConvexHull(bee.experience) +:= convexHull
              println(perExperienceConvexHull)
              beesPositions(bee.id) = List()
          }
        }
      case _ =>
    }
  }

  def calculateExperience(bee: Bee): Experience = if (bee.numberOfFlights == 0) Novice
  else if (bee.numberOfFlights == 3) Experienced else if (bee.numberOfFlights == 5) Expert else bee.experience


  private def calculateConvexHull(bee: Bee): Double = {
    def lineDistance(line: ((Int, Int), (Int, Int))): ((Int, Int)) => Double = {

      line match {
        case ((x1, y1), (x2, y2)) =>

          val divider = Math.sqrt((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1))
          val dy = y2 - y1
          val dx = x2 - x1
          val rest = x2 * y1 - y2 * x1

          (point: (Int, Int)) => (dy * point._1 - dx * point._2 + rest) / divider
      }
    }

    // barycentric coordinate system
    def isInsideTheTriangle(triangle: ((Int, Int), (Int, Int), (Int, Int))): ((Int, Int)) => Boolean = {

      triangle match {
        case ((x1, y1), (x2, y2), (x3, y3)) =>


          val denominator = (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
          val dy2y3 = y2 - y3
          val dx3x2 = x3 - x2
          val dy3y1 = y3 - y1
          val dx1x3 = x1 - x3

          point: (Int, Int) => {
            lazy val a = (dy2y3 * (point._1 - x3) + dx3x2 * (point._2 - y3)) / denominator
            lazy val b = (dy3y1 * (point._1 - x3) + dx1x3 * (point._2 - y3)) / denominator
            lazy val c = 1 - a - b

            (0 <= a && a <= 1) && (0 <= b && b <= 1) && (0 <= c && c <= 1)

          }

      }

    }

    def quickHull(points: List[(Int, Int)]): List[(Int, Int)] = {

      def findHull(set: List[((Int, Int), Double)], point1: (Int, Int), point2: (Int, Int)): List[(Int, Int)] = {
        if (set.isEmpty)
          Nil
        else {

          val (maxDistancePoint, _) = set.foldLeft(set.head) { case (maxDistanceItem, item) =>
            if (Math.abs(item._2) > Math.abs(maxDistanceItem._2))
              item
            else
              maxDistanceItem
          }

          val belongsFunc = isInsideTheTriangle((point1, point2, maxDistancePoint))

          val pointsLeft = set.filter(p => (p._1 != maxDistancePoint) && !belongsFunc(p._1)).map(_._1)

          val distanceSet1Func = lineDistance((point1, maxDistancePoint))
          val set1 = pointsLeft.map(p => (p, distanceSet1Func(p))).filter(_._2 < 0) // to the right of the oriented line

          val distanceSet2Func = lineDistance((maxDistancePoint, point2))
          val set2 = pointsLeft.map(p => (p, distanceSet2Func(p))).filter(_._2 < 0) // to the right of the oriented line

          findHull(set1, point1, maxDistancePoint) ::: (maxDistancePoint :: findHull(set2, maxDistancePoint, point2))

        }
      }


      val leftPoint = points.foldLeft(points.head) { case (min, current) => if (current._1 < min._1) current else min }
      val rightPoint = points.foldLeft(points.head) { case (max, current) => if (current._1 > max._1) current else max }

      val distanceFuncSet1 = lineDistance((leftPoint, rightPoint))
      val pointsWithDistanceSet = points.map(p => (p, distanceFuncSet1(p)))

      val set1 = pointsWithDistanceSet.filter(_._2 < 0) // on the top/right of the line

      val set2 = pointsWithDistanceSet.filter(_._2 > 0) // bottom/left of the line

      (leftPoint :: findHull(set1, leftPoint, rightPoint)) ::: (rightPoint :: findHull(set2, rightPoint, leftPoint))

    }

    val results = quickHull(beesPositions(bee.id))
    val xx = results.map(p => p._1)
    val yy = results.map(p => p._2)
    val overlace: immutable.Seq[(Int, Int)] = xx zip yy.drop(1) ++ yy.take(1)
    val underlace: immutable.Seq[(Int, Int)] = yy zip xx.drop(1) ++ xx.take(1)

    (overlace.map(t => t._1 * t._2).sum - underlace.map(t => t._1 * t._2).sum).abs / 2.0
  }

  private def createFlowerPatches(grid: Grid): Unit = {
    for {x <- 50 to 60; y <- 50 to 60} {
      grid.cells(x)(y) = FlowerPatch.create(initialSignal = Signal.apply(0.4))
    }
  }

  private def spawnBee(grid: Grid): Unit = {
    val bee = Bee.create(id, config.beeSignalInitial)
    id += 1
    grid.cells(31)(31) = bee
    beesPositions(bee.id) = List((31, 31))
    partialDistances(bee.id) = calculateDistance(hivePosition, (30, 30))
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
