package pl.edu.agh.beexplore.algorithm

import java.awt.Color
import java.io.File

import com.avsystem.commons._
import javax.imageio.ImageIO
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model._
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.beexplore.utils.BeeUtils
import pl.edu.agh.beexplore.utils.ColorUtils._
import pl.edu.agh.beexplore.utils.DistanceUtils._
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.Array._
import scala.collection.immutable.TreeSet
import scala.util.Random
import scala.util.control.Breaks._

class BeexploreMovesController(bufferZone: TreeSet[(Int, Int)])
                              (implicit config: BeexploreConfig) extends MovesController {

  import Cell._

  private var grid: Grid = _
  private val random = new Random(System.nanoTime())
  private val beeUtils = new BeeUtils()

  override def initialGrid: (Grid, BeexploreMetrics) = {
    grid = Grid.empty(bufferZone, BeexploreCell(Cell.emptySignal, Vector.empty, Id.Start))


    if (config.flowerPatchesFromFile) {
      val img = ImageIO.read(new File("map_big.png"))
      val w = img.getWidth
      val h = img.getHeight
      val imgFlowerPatch = ofDim[Boolean](w, h)

      //      val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
      for (x <- 0 until w; y <- 0 until h) {
        val c = new Color(img.getRGB(x, y))
        if (colorDistance(c, white) < 1)
          imgFlowerPatch(x)(y) = false
        else if (similarTo(c, red) || similarTo(c, blue) || similarTo(c, yellow)) {
          imgFlowerPatch(x)(y) = true
          if (colorDistance(c, yellow) > colorDistance(c, white))
            imgFlowerPatch(x)(y) = false
        }
      }
      //      ImageIO.write(out, "jpg", new File("test_bettermap.jpg"))

      var newFlowerPatchId = -1
      for (x <- 0 until w; y <- 0 until h) {
        if (x > 0 && y > 0 && x < w - 1 && y < h - 1 && imgFlowerPatch(y)(x)) {
          newFlowerPatchId += 1
          beeUtils.processNeighbourFlowerPatches(x, y, newFlowerPatchId, grid, imgFlowerPatch)
        }
        grid.cells(config.beeColonyCoordinateX)(config.beeColonyCoordinateY) = BeeColony.create(Vector.fill(config.beeNumber)(Bee.create()))
      }

      println("flowerPatchId: ", newFlowerPatchId)

    }
    else {
      //    coordinates are not absolute, now they're for each node
      //    to calculate absoluteCoords, probably will need sth like absoluteX = x + nodeId(horizontally) * gridSize
      /* if (workerId.value == config.beeColonyWorkerId)
         grid.cells(config.beeColonyCoordinateX)(config.beeColonyCoordinateY) = BeeColony.create(Vector.fill(config.beeNumber)(Bee.create()))*/

      for (i: Int <- 0 until config.flowerPatchNumber) {
        var x = random.nextInt(config.gridSize - 3) + 1
        var y = random.nextInt(config.gridSize - 3) + 1

        while (grid.cells(x)(y).isInstanceOf[BeeColony]
          || grid.cells(x)(y).isInstanceOf[BufferCell]
          || grid.cells(x)(y).asInstanceOf[BeexploreCell].flowerPatch != Id.Start
          || grid.cells(x)(y) == Obstacle) {
          print(".")
          x = random.nextInt(config.gridSize - 3) + 1
          y = random.nextInt(config.gridSize - 3) + 1
        }

        val flowerPatchId = config.flowerPatchNumber + i
        grid.cells(x)(y) = BeexploreCell(Cell.emptySignal + config.flowerPatchSignalMultiplier, Vector.empty, Id(flowerPatchId))

        val flowerPatchSize = random.nextInt(config.flowerPatchSizeMax - config.flowerPatchSizeMin) + config.flowerPatchSizeMin
        println("generating flowerPatch ", flowerPatchId, " | flowerpatch size: ", flowerPatchSize)

        for (_ <- 0 until flowerPatchSize) {
          val possibleNextFlower = Iterator(Random.shuffle(List((-1, 0), (0, -1), (0, 1), (1, 0)))).flatten
          var nextFlower = (0, 0)
          var newX = x + nextFlower._1
          var newY = y + nextFlower._2
          breakable {
            while (possibleNextFlower.hasNext) {
              nextFlower = possibleNextFlower.next()
              newX = x + nextFlower._1
              newY = y + nextFlower._2

              val cell = grid.cells(newX)(newY)
              if (cell != Obstacle
                && !cell.isInstanceOf[BufferCell]
                && !cell.isInstanceOf[BeeColony]
                && cell.asInstanceOf[BeexploreCell].flowerPatch == Id.Start
              ) {
                grid.cells(newX)(newY) = BeexploreCell(Cell.emptySignal + config.flowerPatchSignalMultiplier, Vector.empty, Id(flowerPatchId))
                x = newX
                y = newY
                break
              }
            }
          }
        }
      }
    }

    println("grid initialized")

    val metrics = BeexploreMetrics(
      config.beeNumber,
      config.flowerPatchNumber
    )
    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, BeexploreMetrics) = {

    this.grid = grid
    val newGrid = Grid.empty(bufferZone, BeexploreCell.create())

    var firstTripFlowerPatchCount = Map.empty[Id, (Int, Double)]
    var discoveredFlowerPatchCount = Map.empty[Id, (Int, Double)]
    var beeMoves = 0L
    var beeTrips = 0L
    //    var returningBees = 0L

    newGrid.cells(config.beeColonyCoordinateX)(config.beeColonyCoordinateY) = grid.cells(config.beeColonyCoordinateX)(config.beeColonyCoordinateY) match {
      case cell: BeeColony => cell.copy(bees = Vector.empty)
      case cell: BeexploreCell => cell.copy(bees = Vector.empty)
    }

    def adjustSmell(bees: Vector[Bee], flowerPatchValue: Int): Signal = (config.beeInitialSignal * bees.size) + (if (flowerPatchValue != -1)
      config.flowerPatchSignalMultiplier else Signal(0))

    def update[T](x: Int, y: Int)(updater: T => T): Unit = {
      newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
        case cell: BeexploreCell => updater(cell.asInstanceOf[T]).asInstanceOf[BeexploreCell]
        case cell: BeeColony => updater(cell.asInstanceOf[T]).asInstanceOf[BeeColony]
        case BufferCell(cell: BeexploreCell) => BufferCell(updater(cell.asInstanceOf[T]).asInstanceOf[BeexploreCell])
        case Obstacle => Obstacle
      }
    }

    def calculateCell(x: Int, y: Int): Unit = {
      def calculateCurrentResultAndPendingMoves(previousResult: Iterator[Bee], pendingMoves: Map[(Int, Int), Stream[Bee]],
                                                bee: Bee): (Iterator[Bee], Map[(Int, Int), Stream[Bee]]) = {
        val action = moveBee(bee, x, y, pendingMoves)
        //                only 1 move in moves iterator
        val actionMoves = action.moves.map { case ((x, y), movingBee) =>
          (x, y) -> (pendingMoves((x, y)) :+ movingBee)
        }
        val updatedPendingMoves = (actionMoves ++ pendingMoves.iterator.filter { case (key, _) =>
          !actionMoves.exists { case (secondKey, _) => key == secondKey }
        }).toMap
        (previousResult ++ action.currentCellResult, updatedPendingMoves)
      }

      def calculateNewBeesAndMoves(bees: Vector[Bee]): (Iterator[Bee], Map[(Int, Int), Stream[Bee]]) = bees.foldLeft((Iterator[Bee](),
        Map.empty[(Int, Int), Stream[Bee]].withDefaultValue(Stream.empty))) { case ((currentCellResult, pendingMoves), bee) =>
        calculateCurrentResultAndPendingMoves(currentCellResult, pendingMoves, bee)
      }

      def updateGridForMoves(moves: BMap[(Int, Int), Stream[Bee]]): Unit = moves.foreach { case ((i, j), bees) =>
        //            movesCount += bees.size
        update[BeexploreCell](i, j) { cell =>
          val newBees = cell.bees ++ bees
          val smellAdjustment = adjustSmell(newBees, cell.flowerPatch.value)
          cell.copy(smell = cell.smell + smellAdjustment, bees = newBees)
        }
        update[BeeColony](i, j)(cell => cell.copy(bees = cell.bees ++ bees))
      }

      this.grid.cells(x)(y) match {
        case Obstacle =>
        case BufferCell(BeexploreCell(smell, _, _)) =>
          update[BeexploreCell](x, y)(cell => cell.copy(smell = (cell.smell + smell) + adjustSmell(cell.bees, cell.flowerPatch.value)))
        case BeeColony(_, _, bees, _, _, _, _) =>
          val (_, moves: BMap[(Int, Int), Stream[Bee]]) = calculateNewBeesAndMoves(bees)
          updateGridForMoves(moves)
        case BeexploreCell(smell, bees, _) =>
          val (newBees: Iterator[Bee], moves: BMap[(Int, Int), Stream[Bee]]) = calculateNewBeesAndMoves(bees)

          import Cell._
          update[BeexploreCell](x, y) { cell =>
            val flowerPatch = grid.cells(x)(y).asInstanceOf[BeexploreCell].flowerPatch
            val bees = cell.bees ++ newBees
            val newSmell = cell.smell + smell
            val smellAdjustment = adjustSmell(bees, flowerPatch.value)
            cell.copy(
              smell = newSmell + smellAdjustment,
              bees = bees,
              flowerPatch = flowerPatch,
            )
          }
          updateGridForMoves(moves)
      }
    }

    def moveBee(bee: Bee, x: Int, y: Int, moves: Map[(Int, Int), Stream[Bee]]): BeeAction = {

      val (newX, newY) = MovementStrategy.makeMove(bee, x, y, moves, grid, config)

      def getUpdatedBee(cell: GridPart, bee: Bee): Bee = {
        val moveVectorX = newX - x
        val moveVectorY = newY - y

        var tripNumber = bee.tripNumber
        var destination = bee.destination
        var maxTripDuration = bee.maxTripDuration - 1
        var discoveredFlowerPatches = bee.discoveredFlowerPatches
        var vectorFromColony = (bee.vectorFromColony._1 + moveVectorX, bee.vectorFromColony._2 + moveVectorY)
        beeMoves += 1
        val lastMoveVector = (moveVectorX, moveVectorY)
        var randomStepsLeft = math.max(bee.randomStepsLeft - 1, 0)

        //        println("[BEE] ", bee, " moving from (",x, y, ") to (", newX, newY, ")")

        cell match {
          case BeexploreCell(_, _, flowerPatch) => {
            if (destination == vectorFromColony)
            // destination found - bee can fly wherever it wants
              destination = (Int.MinValue, Int.MinValue)
            if (flowerPatch != Id.Start) {
              randomStepsLeft = config.stepsWithNoSmellAfterPatchDiscovery
              if (!bee.discoveredFlowerPatches.contains(flowerPatch))
                discoveredFlowerPatches += flowerPatch -> vectorFromColony
            }
            if (maxTripDuration <= 0)
            //              same as negated vectorFromColony (but then desiredMoveCoords wouldn't work)
              destination = (0, 0)
          }

          case colony@BeeColony((x_c, y_c), _, _, firstTripDetections, discoveredFlowerPatchCoords, discoveredFlowerPatchMetrics, returningBees) => {
            //            println("bee in colony, discovered FlowerPatches: ", bee.discoveredFlowerPatches)
            // flowerPatch detection probabilities on 1st scouting trip
            if (bee.tripNumber == 1) {
              val updatesFirstTripDetections = bee.discoveredFlowerPatches.iterator.map { case (id, _) =>
                val discoveredFlowerPatchDistance = math.sqrt(math.pow(bee.discoveredFlowerPatches(id)._1, 2) + math.pow(bee.discoveredFlowerPatches(id)._2, 2))
                val (detection, distance) = firstTripDetections.getOrElse(id, (0, 0.toDouble))
                val updatedDistance = if (distance > discoveredFlowerPatchDistance) discoveredFlowerPatchDistance else distance
                id -> (detection + 1, updatedDistance)
              }
              firstTripFlowerPatchCount = (updatesFirstTripDetections ++ firstTripDetections.iterator.filter { case (firstId, _) => updatesFirstTripDetections.exists { case (id, _) => firstId == id } }).toMap
            }
            val newReturningBees = if (!returningBees.contains(0)) returningBees + (0 -> 1) else returningBees + (0 -> (returningBees(0) + 1))
            // newest coord for each flowerPatch are kept in BeeColony (since potentially the environment could dynamically change)
            val newDiscoveredFlowerPatchCords = discoveredFlowerPatchCoords ++ bee.discoveredFlowerPatches
            val beeDiscoveredFlowerUpdates = bee.discoveredFlowerPatches.iterator.map { case (id, _) =>
              val discoveredFlowerPatchDistance = math.sqrt(math.pow(bee.discoveredFlowerPatches(id)._1, 2) + math.pow(bee.discoveredFlowerPatches(id)._2, 2))
              id -> (discoveredFlowerPatchMetrics(id)._1 + 1, discoveredFlowerPatchDistance)
            }
            val newDiscoveredFlowerPatchMetrics = (beeDiscoveredFlowerUpdates ++ discoveredFlowerPatchMetrics.iterator.filter { case (firstId, _) => beeDiscoveredFlowerUpdates.exists { case (id, _) => firstId == id } }).toMap
            tripNumber += 1
            maxTripDuration = config.beeTripDuration
            vectorFromColony = (0, 0)

            config.beeSearchMode match {
              case 1 => //  bee colony
                destination = (Int.MinValue, Int.MinValue)
              case 2 => // recruitment
                val possibleDestinations = newDiscoveredFlowerPatchCords.values.toList
                if (possibleDestinations.nonEmpty)
                  destination = possibleDestinations(random.nextInt(possibleDestinations.length))
                else
                  destination = (Int.MinValue, Int.MinValue)
              case 3 => // Bees has random search
                destination = (random.nextInt(config.gridSize - 1), random.nextInt(config.gridSize - 1))
            }
            println("discoveredFlowerPatchMetrics: ", discoveredFlowerPatchMetrics, "returnedBees: ", returningBees)

            if (beeTrips < tripNumber)
              beeTrips = tripNumber
            discoveredFlowerPatchCount = discoveredFlowerPatchMetrics
            grid.cells(x_c)(y_c) = colony.copy(firstTripDetections = firstTripFlowerPatchCount,
              discoveredFlowerPatchMetrics = newDiscoveredFlowerPatchMetrics, returningBees = newReturningBees, discoveredFlowerPatchCoords = newDiscoveredFlowerPatchCords)
          }

          case _ =>
        }

        bee.copy(tripNumber, maxTripDuration, Map.empty, destination, vectorFromColony, lastMoveVector, randomStepsLeft)
      }

      val updatedBee = getUpdatedBee(this.grid.cells(newX)(newY), bee)

      val destination = Iterator(((newX, newY), updatedBee))

      BeeAction(Iterator.empty, moves = destination)
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } calculateCell(x, y)

    //    todo w każdym kroku zapewnić uzupełnienie metryk
    val metrics = BeexploreMetrics(
      beeCount = config.beeNumber,
      flowerPatchCount = config.flowerPatchNumber,
      firstTripFlowerPatchCount = firstTripFlowerPatchCount,
      discoveredFlowerPatchCount = discoveredFlowerPatchCount,
      beeMoves = beeMoves,
      beeTrips = beeTrips
    )

    (newGrid, metrics)
  }
}
