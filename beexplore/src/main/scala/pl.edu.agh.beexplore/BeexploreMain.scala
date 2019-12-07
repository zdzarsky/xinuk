package pl.edu.agh.beexplore

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.beexplore.algorithm.BeexploreMovesController
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.parallel.BeexploreConflictResolver
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, SmellingCell}

object BeexploreMain extends LazyLogging{
  private val configPrefix = "beexplore"
  private val metricHeaders = Vector(
    "exampleMetrics",
    "anotherExampleMetrics"
  )

  private def cellToColor(cell: SmellingCell): Color = {
    cell match {
      case Bee(_,_,_,_) => Color.YELLOW;
      case FlowerPatch(_) => Color.PINK
      case Beehive(_) => Color.BLACK
      case _ => Color.WHITE
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[BeexploreConfig](configPrefix, metricHeaders, BeexploreConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard)(new BeexploreMovesController(_)(_),
      { case cell: SmellingCell => cellToColor(cell) }
    ).start()
  }
}
