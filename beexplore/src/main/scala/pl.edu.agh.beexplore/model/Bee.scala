package pl.edu.agh.beexplore.model

import pl.edu.agh.beexplore.config.BeexploreConfig


final case class Bee(tripNumber: Int = 1,
                      maxTripDuration: Long, //lifespan -> maxTripDuration
                      discoveredFlowerPatches: Map[Id, (Int, Int)] = Map.empty,
                      destination: (Int, Int) = (Int.MinValue, Int.MinValue),
                      vectorFromColony: (Int, Int) = (0, 0),
                      lastMoveVector: (Int, Int) = (Int.MinValue, Int.MinValue),
                      randomStepsLeft: Int = 0)

object Bee {
  def create()(implicit config: BeexploreConfig): Bee = Bee(maxTripDuration = config.beeTripDuration)
}