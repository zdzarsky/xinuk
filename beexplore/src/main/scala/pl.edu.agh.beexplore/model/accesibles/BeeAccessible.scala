package pl.edu.agh.beexplore.model.accesibles

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.Bee
import pl.edu.agh.beexplore.model.Bee.BeeRole
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart}

trait BeeAccessible[+T <: GridPart] {
  def withBee(exp: Int, hunger: Int, role: BeeRole): T
}

object BeeAccessible {

  def unapply(part: GridPart)(
    implicit config: BeexploreConfig): Option[BeeAccessible[GridPart]] =
    part match {
      case cell: EmptyCell => Some(unapply(cell))
      case cell: BufferCell => Some(unapply(cell))
      case _: GridPart => None
    }

  def unapply(arg: EmptyCell)(implicit config: BeexploreConfig): BeeAccessible[Bee] = {
    (exp: Int, hunger: Int, role: BeeRole) => Bee(arg.smellWith(config.beeSignalInitial), exp, hunger, role)
  }

  def unapply(arg: BufferCell)(implicit config: BeexploreConfig): BeeAccessible[BufferCell] =
    (exp: Int, hunger: Int, role: BeeRole) => BufferCell(Bee(arg.smellWith(config.beeSignalInitial), exp, hunger, role))
}
