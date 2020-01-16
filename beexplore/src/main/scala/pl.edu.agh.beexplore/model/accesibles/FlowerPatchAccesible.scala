package pl.edu.agh.beexplore.model.accesibles

import pl.edu.agh.beexplore.model.{Bee, FlowerPatch}
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart}

trait FlowerPatchAccesible[+T <: GridPart] {
  def withFlowers(smell: SmellArray): T
}

object FlowerPatchAccesible {
  def unapply(part: GridPart): Option[FlowerPatchAccesible[GridPart]] = part match {
    case cell: BufferCell => Some(unapply(cell))
    case cell: Bee => Some(unapply(cell))
    case _: GridPart => None
  }


  def unapply(arg: BufferCell): FlowerPatchAccesible[BufferCell] =
    (smellArray) => BufferCell(FlowerPatch(smellArray))

  def unapply(arg: Bee): FlowerPatchAccesible[Bee] =
    (smellArray) => arg.copy(smell =
      arg.smell.zip(smellArray).map {
        case (x, y) => x.zip(y).map {
          case (j, k) => j + k
        }
      })
}




