package pl.edu.agh.beexplore.model.accesibles

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.Bee
import pl.edu.agh.beexplore.model.Bee.Experience
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart}

trait BeeAccessible[+T <: GridPart] {
  def withBee(id: Int, numberOfFlights: Int, experience: Experience, hunger: Int): T
}

object BeeAccessible {

  def unapply(part: GridPart)(
    implicit config: BeexploreConfig): Option[BeeAccessible[GridPart]] = part match {
      case cell: EmptyCell => Some(unapply(cell))
      case cell: BufferCell => Some(unapply(cell))
      case _: GridPart => None
    }

  def unapply(arg: EmptyCell)(implicit config: BeexploreConfig): BeeAccessible[Bee] = {
    (id: Int, numberOfFlights: Int, experience: Experience, hunger: Int) => Bee(id, arg.smellWith(config.beeSignalInitial), numberOfFlights, experience, hunger)
  }

  def unapply(arg: BufferCell)(implicit config: BeexploreConfig): BeeAccessible[BufferCell] =
    (id: Int, numberOfFlights: Int, experience: Experience, hunger: Int) => BufferCell(Bee(id, arg.smellWith(config.beeSignalInitial), numberOfFlights, experience, hunger))
}
