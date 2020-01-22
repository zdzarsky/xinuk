package pl.edu.agh.beexplore.utils

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.xinuk.model.{Grid, Signal}

class IdealWorld(implicit config: BeexploreConfig) extends HoneyWorld {

  private val hivePosition: (Int, Int) = (config.beehiveX, config.beehiveY)
  val hive: Beehive = Beehive.create(Signal.Zero, hivePosition)

  override def create(grid: Grid): Unit  ={
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

  private def spawnBee(grid: Grid, config: BeexploreConfig): Unit = {
    grid.cells(20)(20) = Bee.create(config.beeSignalInitial)
    grid.cells(15)(15) = Bee.create(config.beeSignalInitial)
    grid.cells(29)(60) = Bee.create(config.beeSignalInitial)
  }

  private def createBeehive(grid: Grid, config: BeexploreConfig): Unit = {
    grid.cells(config.beehiveX)(config.beehiveY) = hive
  }
}
