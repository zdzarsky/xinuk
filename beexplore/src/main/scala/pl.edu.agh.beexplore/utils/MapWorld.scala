package pl.edu.agh.beexplore.utils
import org.slf4j.LoggerFactory
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.Bee.Novice
import pl.edu.agh.beexplore.model.{Bee, Beehive}
import pl.edu.agh.xinuk.model.{Grid, Signal}

import scala.util.{Failure, Success}

class MapWorld(implicit config: BeexploreConfig) extends HoneyWorld {

  val logger = LoggerFactory.getLogger(getClass.toString)
  var hiveInstance: Beehive = Beehive.create(Signal.Zero, (0, 0), Vector.empty)

  override def create(grid: Grid): Unit = {
    MapReader.read(config.mapPath, grid) match {
      case Success(grid) =>
        for{
          x <- grid.cells.indices
          y <- grid.cells.indices
        }{
          grid.cells(x)(y) match {
            case Beehive(_, position, _) =>
              hiveInstance = Beehive.create(Signal.Zero, position, (0 until config.beesCount)
                .map(id => Bee.create(id, config.beeSignalInitial).withExperience(Novice).withHunger(id)).toVector)
            case _ =>
          }
        }
      case Failure(exception) => logger.error(s"Unable to read map, error message: ${exception.getMessage}")
    }
  }

  override def hive(): Beehive = hiveInstance

  override def updateHive(hive: Beehive): Unit = hiveInstance = hive

  override def beeIds(): Seq[Int] = (0 until config.beesCount)
}
