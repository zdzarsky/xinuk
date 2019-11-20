package pl.edu.agh.beexplore.algorithm

import com.avsystem.commons._
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.Bee
import pl.edu.agh.xinuk.model.{Grid, Obstacle, Signal}

import scala.math.signum
import scala.util.Random

sealed trait MovementStrategy {
  def makeMove(bee: Bee, x: Int, y: Int, moves: Map[(Int, Int), Stream[Bee]], grid: Grid, config: BeexploreConfig): (Int, Int)
}

object MovementStrategy extends MovementStrategy {

  private case object SmellBasedMovementStrategy extends MovementStrategy {
    override def makeMove(bee: Bee, x: Int, y: Int, moves: Map[(Int, Int), Stream[Bee]], grid: Grid, config: BeexploreConfig): (Int, Int) = {
      val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)

      def isNotObstacle(coords: (Int, Int)): Boolean = grid.cells(coords._1)(coords._2) != Obstacle

      /*val destination =*/ Grid.SubcellCoordinates
        .map {
          case (i, j) =>
            grid.cells(x)(y).smell(i)(j) + moves.get((x, y)).map(
              bees => config.beeInitialSignal * bees.size).getOrElse(Signal.Zero)
        }
        .zipWithIndex
        .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
        .iterator
        .collect { case (_, idx) if !isNotObstacle(neighbourCellCoordinates(idx)) =>
          val (i, j) = neighbourCellCoordinates(idx)
          (i, j)
        }
        .nextOpt
        .getOrElse((x, y))
      /*      if (destination == (config.beeColonyCoordinateX,config.beeColonyCoordinateY))
              randomMoveCoords(x, y)
            else
              destination*/
    }
  }

  private case object RandomMovementStrategy extends MovementStrategy {
    private val random = new Random(System.nanoTime())

    @scala.annotation.tailrec
    override def makeMove(bee: Bee, x: Int, y: Int, moves: Map[(Int, Int), Stream[Bee]], grid: Grid, config: BeexploreConfig): (Int, Int) = {
      var newX = x + random.nextInt(3) - 1
      var newY = y + random.nextInt(3) - 1

      if (grid.cells(newX)(newY) == Obstacle) {
        newX = x
        newY = y
      }

      if ((newX, newY) == (config.beeColonyCoordinateX, config.beeColonyCoordinateY)) {
        //        println("pszczola losuje again")
        makeMove(bee, x, y, moves, grid, config)
      }
      else
        (newX, newY)
    }
  }

  private case object DesiredMovementStrategy extends MovementStrategy {
    override def makeMove(bee: Bee, x: Int, y: Int, moves: Map[(Int, Int), Stream[Bee]], grid: Grid, config: BeexploreConfig): (Int, Int) = {
      val newX = x + signum(bee.destination._1 - bee.vectorFromColony._1)
      val newY = y + signum(bee.destination._2 - bee.vectorFromColony._2)
      (newX, newY)
    }
  }

  private case object TurningAnglesMovementStrategy extends MovementStrategy {
    private val random = new Random(System.nanoTime())

    override def makeMove(bee: Bee, x: Int, y: Int, moves: Map[(Int, Int), Stream[Bee]], grid: Grid, config: BeexploreConfig): (Int, Int) = {

      val turningVector = random.nextInt(214) match {
        case x if 0 until 58 contains x =>
          (0, 1) //fly straight
        case x if 58 until 97 contains x =>
          (1, 1) // 45 degrees
        case x if 97 until 133 contains x =>
          (1, 0) // 90 degrees
        case x if 133 until 178 contains x =>
          (1, -1) // 135 degrees
        case x if 178 until 214 contains x =>
          (0, -1) // 180 degrees - turn around
      }

      val (newX, newY) = (bee.lastMoveVector._1, bee.lastMoveVector._2 * -1) match {
        case (0, 1) =>
          (x + turningVector._1, y + turningVector._2)
        case (1, 0) =>
          (x + turningVector._2, y - turningVector._1)
        case (0, -1) =>
          (x - turningVector._1, y - turningVector._2)
        case (-1, 0) =>
          (x - turningVector._2, y + turningVector._1)
        case (1, 1) =>
          (x + math.signum(turningVector._1 + turningVector._2), y + math.signum(turningVector._2 - turningVector._1))
        case (-1, -1) =>
          (x - math.signum(turningVector._1 + turningVector._2), y - math.signum(turningVector._2 - turningVector._1))
        case (1, -1) =>
          (x + math.signum(turningVector._2 - turningVector._1), y - math.signum(turningVector._1 + turningVector._2))
        case (-1, 1) =>
          (x - math.signum(turningVector._1 + turningVector._2), y + math.signum(turningVector._2 - turningVector._1))
        case (0, 0) =>
          (x + turningVector._1, y + turningVector._2)
        case (_, _) =>
          (x, y)
      }

      if (grid.cells(newX)(newY) == Obstacle || (newX == config.beeColonyCoordinateX && newY == config.beeColonyCoordinateY)) {
        (x, y)
      }
      else
        (newX, newY)
    }
  }

  override def makeMove(bee: Bee, x: Int, y: Int, moves: Map[(Int, Int), Stream[Bee]], grid: Grid, config: BeexploreConfig): (Int, Int) = {
    bee.destination match {
      case (Int.MinValue, Int.MinValue) =>
        if (config.signalSpeedRatio > 0 && bee.randomStepsLeft == 0)
          SmellBasedMovementStrategy.makeMove(bee, x, y, moves, grid, config)
        else if (bee.lastMoveVector == (Int.MinValue, Int.MinValue))
          RandomMovementStrategy.makeMove(bee, x, y, moves, grid, config) // without smell
        else
          TurningAnglesMovementStrategy.makeMove(bee, x, y, moves, grid, config)

      case _ =>
        DesiredMovementStrategy.makeMove(bee, x, y, moves, grid, config)
    }
  }
}
