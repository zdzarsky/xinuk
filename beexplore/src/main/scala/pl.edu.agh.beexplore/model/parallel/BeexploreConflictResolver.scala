package pl.edu.agh.beexplore.model.parallel

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.model.{GridPart, SmellingCell}
import pl.edu.agh.xinuk.model.parallel.ConflictResolver
import pl.edu.agh.xinuk.simulation.Metrics

object BeexploreConflictResolver extends ConflictResolver[BeexploreConfig] {
  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: BeexploreConfig): (GridPart, Metrics) = {
    (current, BeexploreMetrics(0,0))
  }
}
