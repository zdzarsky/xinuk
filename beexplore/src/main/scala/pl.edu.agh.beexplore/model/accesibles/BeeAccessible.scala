package pl.edu.agh.beexplore.model.accesibles

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{Bee, FlowerPatch}
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart}

trait BeeAccessible[+T <: GridPart] {
  def withBee(exp: Int, hunger: Int) : T
}

object BeeAccessible {

  def unapply(part: GridPart)(
    implicit config: BeexploreConfig): Option[BeeAccessible[GridPart]] = part match {
      case cell: EmptyCell => Some(unapply(cell))
      case cell: BufferCell => Some(unapply(cell))
      case _: GridPart => None
    }

  def unapply(arg: EmptyCell)(implicit config: BeexploreConfig): BeeAccessible[Bee] = {
    (exp: Int, hunger: Int) => Bee(arg.smellWith(config.beeSignalInitial), exp, hunger)
  }

  def unapply(arg: BufferCell)(implicit config: BeexploreConfig): BeeAccessible[BufferCell] =
    (exp: Int, hunger: Int) => BufferCell(Bee(arg.smellWith(config.beeSignalInitial), exp, hunger))

}
