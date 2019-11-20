package pl.edu.agh.beexplore.model

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, SmellingCell}

final case class BeeColony(coordinates: (Int, Int),
                           smell: SmellArray = Cell.emptySignal,
                           bees: Vector[Bee],
                           firstTripDetections: Map[Id, (Int, Double)] = Map.empty,
                           discoveredFlowerPatchCoords: Map[Id, (Int, Int)] = Map.empty,
                           discoveredFlowerPatchMetrics: Map[Id, (Int, Double)] = Map.empty,
                           returningBees: Map[Int, Int] = Map.empty
                          ) extends SmellingCell {
  override type Self = BeeColony

  override def withSmell(smell: SmellArray): BeeColony = copy(smell = smell)
}

object BeeColony {
  def create(bees: Vector[Bee] = Vector.empty)(implicit config: BeexploreConfig): BeeColony =
    BeeColony((config.beeColonyCoordinateX, config.beeColonyCoordinateY), Cell.emptySignal, bees)
}