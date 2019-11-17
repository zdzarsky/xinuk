package pl.edu.agh.beexplore.simulation

import pl.edu.agh.beexplore.model.Id
import pl.edu.agh.xinuk.simulation.Metrics


final case class BeexploreMetrics(beeCount: Int = 0,
                                  flowerPatchCount: Int = 0,
                                  firstTripFlowerPatchCount: Map[Id, (Int, Double)] = Map.empty,
                                  discoveredFlowerPatchCount: Map[Id, (Int, Double)] = Map.empty,
                                  beeMoves: Long = 0,
                                  beeTrips: Long = 0) extends Metrics {

  override def log: String = {
    s"$beeCount;$flowerPatchCount;$discoveredFlowerPatchCount;$beeMoves;$beeTrips"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Bees" -> beeCount,
    "FlowerPatches" -> flowerPatchCount
  )

  override def +(other: Metrics): BeexploreMetrics = {
    other match {
      case BeexploreMetrics.EMPTY => this
      case BeexploreMetrics(otherBeeCount, otherFlowerPatchCount, _, otherDiscoveredFlowerPatchCount, otherBeeMoves,
      otherBeeTrips) =>
        BeexploreMetrics(
          beeCount + otherBeeCount,
          flowerPatchCount + otherFlowerPatchCount,
          firstTripFlowerPatchCount,
          discoveredFlowerPatchCount ++ otherDiscoveredFlowerPatchCount,
          beeMoves + otherBeeMoves,
          beeTrips + otherBeeTrips
        )
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-BeexploreMetrics to BeexploreMetrics")
    }
  }
}

object BeexploreMetrics {
  private val EMPTY = BeexploreMetrics()
  def empty(): BeexploreMetrics = EMPTY
}