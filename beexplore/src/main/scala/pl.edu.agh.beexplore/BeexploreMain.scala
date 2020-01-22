package pl.edu.agh.beexplore

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.beexplore.algorithm.BeexploreMovesController
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.parallel.BeexploreConflictResolver
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.Grid.CellArray
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Grid, Signal, SmellingCell}

object BeexploreMain extends LazyLogging {
  private val configPrefix = "beexplore"
  private val metricHeaders = Vector(
    "exampleMetrics",
    "anotherExampleMetrics"
  )

  private def cellToColor(cell: SmellingCell): Color = {
    cell match {
      case Bee(_, _, _, _, _) => Color.BLUE;
      case FlowerPatch(_) => Color.PINK
      case Beehive(_, _, _) => Color.BLACK
      case cell: SmellingCell => debugSmell(cell)
      case _ => Color.WHITE
    }
  }

  private def debugSmell(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
    val brightness = Math.pow(smellValue, 0.1).toFloat
    if (smellValue < 0.00001) {
      val hue = 1f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.001) {
      val hue = 0.65f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.1) {
      val hue = 0.28f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else {
      val hue = 0.11f
      val saturation = 0.69f
      Color.getHSBColor(hue, saturation, brightness)
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[BeexploreConfig](configPrefix, metricHeaders, BeexploreConflictResolver, DefaultSmellPropagation.calculateSmellAddendsCircular)(new BeexploreMovesController(_)(_),
      { case cell: SmellingCell => cellToColor(cell) }
    ).start()
  }
}
