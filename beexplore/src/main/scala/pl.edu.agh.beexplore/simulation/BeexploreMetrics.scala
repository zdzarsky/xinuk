package pl.edu.agh.beexplore.simulation

import pl.edu.agh.xinuk.simulation.Metrics

case class BeexploreMetrics() extends Metrics {
  override def log: String = {
    "xD"
  }

  override def series: Vector[(String, Double)] = Vector(
    "exampleMetrics" -> 0,
    "anotherExampleMetrics" -> 0
  )

  override def +(other: Metrics): Metrics = {
    BeexploreMetrics()
  }
}
