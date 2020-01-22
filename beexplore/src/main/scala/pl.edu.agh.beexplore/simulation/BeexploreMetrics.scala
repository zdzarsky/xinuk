package pl.edu.agh.beexplore.simulation

import pl.edu.agh.beexplore.model.Bee
import pl.edu.agh.beexplore.model.Bee.Experience
import pl.edu.agh.xinuk.simulation.Metrics

final case class BeexploreMetrics(/*
                                   beesPositions: Map[Bee, Seq[(Int, Int)]],
                                   partialDistances: Map[Bee, Double],
                                   perExperienceFlightDistance: Map[Experience, Seq[Double]],
                                   perExperienceConvexHull: Map[Experience, Seq[Double]]*/
                                  metric1:Double,
                                  metric2:Double) extends Metrics {
  override def log: String = s"$metric1;$metric2"

  override def series: Vector[(String, Double)] = Vector(
    "metric1" -> metric1,
    "metric2" -> metric2
  )

  override def +(other: Metrics): Metrics = other match {
    case BeexploreMetrics.EMPTY => this
    case null => this
    case BeexploreMetrics(otherMetric1, otherMetric2) =>
      BeexploreMetrics(metric1 + otherMetric1, metric2 + otherMetric2)
    case _ => throw new UnsupportedOperationException(s"Cannot add: non-BeexploreMetrics to BeexploreMetrics")
  }
}

object BeexploreMetrics {
  private val EMPTY = BeexploreMetrics(0,0)

  def empty: BeexploreMetrics = EMPTY
}
