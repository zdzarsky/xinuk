package pl.edu.agh.beexplore.utils

import com.avsystem.commons.SharedExtensions._
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.xinuk.model.{Grid, Signal}

class IdealWorld(implicit config: BeexploreConfig) extends HoneyWorld {

  private var id = 0

  private val hivePosition: (Int, Int) = (config.beehiveX, config.beehiveY)
  val hive: Beehive = Beehive.create(Signal.Zero, hivePosition)

  override def create(grid: Grid): Seq[Int] = {
    createBeehive(grid, config)
    createFlowerPatches(grid)
    spawnBee(grid, config)
  }

  private def createFlowerPatches(grid: Grid): Unit = {
    for {x <- 50 to 60; y <- 50 to 60} {
      grid.cells(x)(y) = FlowerPatch.create(initialSignal = Signal.apply(0.4))
    }
    for {x <- 60 to 70; y <- 0 to 30} {
      grid.cells(x)(y) = FlowerPatch.create(initialSignal = Signal.apply(0.4))
    }
    for {x <- 0 to 30; y <- 60 to 70} {
      grid.cells(x)(y) = FlowerPatch.create(initialSignal = Signal.apply(0.4))
    }
  }

  private def spawnBee(grid: Grid, config: BeexploreConfig): Seq[Int] = {
    val positions = Seq((20, 20), (15, 15), (29, 60))
    (0 to 2).setup(_.foreach { beeId =>
      val (x, y) = positions(beeId)
      grid.cells(x)(y) = Bee.create(beeId, config.beeSignalInitial)
    })
  }

  private def createBeehive(grid: Grid, config: BeexploreConfig): Unit = {
    grid.cells(config.beehiveX)(config.beehiveY) = hive
  }
}
