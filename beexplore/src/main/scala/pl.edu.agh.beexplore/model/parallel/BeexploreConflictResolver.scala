package pl.edu.agh.beexplore.model.parallel

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.BeexploreCell
import pl.edu.agh.beexplore.simulation.BeexploreMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object BeexploreConflictResolver extends ConflictResolver[BeexploreConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: BeexploreConfig): (GridPart, BeexploreMetrics) = {
    (current, incoming) match {
      case (Obstacle, _) => (Obstacle, BeexploreMetrics.empty())
      case (BeexploreCell(currentSmell, currentBees, currentId), BeexploreCell(incomingSmell, incomingBees, _)) =>
        (BeexploreCell(currentSmell + incomingSmell, currentBees ++ incomingBees, currentId), BeexploreMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
