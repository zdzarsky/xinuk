package pl.edu.agh.beexplore

import java.awt.Color

import pl.edu.agh.beexplore.algorithm.BeexploreMovesController
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{BeeColony, BeexploreCell, Id}
import pl.edu.agh.beexplore.model.parallel.BeexploreConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.DefaultSmellPropagation

object BeexploreMain {
  private val configPrefix = "beexplore"
  private val metricHeaders = Vector(
    "beeCount",
    "flowerPatchCount",
    "firstTripFlowerPatchCount",
    "discoveredFlowerPatchCount",
    "beeMoves",
    "beeTrips"
  )

  private def cellToColor(cell: BeexploreCell): Color = {
    val hue = 0.15f
    val saturation = 0.8f
    val luminance = cell.bees.size
    Color.getHSBColor(hue, saturation, luminance)
  }

  private def colonyCellToColor(cell: BeeColony): Color = {
    val hue = 0.01f
    val saturation = 1f
    val luminance = 0.5f + cell.bees.size/100
    Color.getHSBColor(hue, saturation, luminance)
  }

  private def flowerPatchCellToColor(cell: BeexploreCell): Color = {
    val hue = 0.25f + (0.01f * cell.flowerPatch.value)
    val saturation = 1f
    val luminance = 0.5f
    Color.getHSBColor(hue, saturation, luminance)
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[BeexploreConfig](
      configPrefix,
      metricHeaders,
      BeexploreConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard,
      BeexploreCell.create()
    )(
      new BeexploreMovesController(_, _)(_),
      {
        case cell: BeexploreCell =>
          if (cell.flowerPatch != Id.Start && cell.bees.isEmpty) flowerPatchCellToColor(cell)
          else cellToColor(cell)
        case cell: BeeColony => colonyCellToColor(cell)
      }
    ).start()
  }
}