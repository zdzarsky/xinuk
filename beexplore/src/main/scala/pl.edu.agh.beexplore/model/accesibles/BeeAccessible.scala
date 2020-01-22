package pl.edu.agh.beexplore.model.accesibles

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{Bee, FlowerPatch}
import pl.edu.agh.beexplore.model.Bee.Experience
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, Signal}

trait BeeAccessible[+T <: GridPart] {
  def withBee(smell: Signal, id: Int, numberOfFlights: Int, experience: Experience, hunger: Int): T
}

object BeeAccessible {

  def unapply(part: GridPart)(
    implicit config: BeexploreConfig): Option[BeeAccessible[GridPart]] = part match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case cell: FlowerPatch => Some(unapply(cell))
    case _: GridPart => None
  }

  def unapply(arg: EmptyCell)(implicit config: BeexploreConfig): BeeAccessible[Bee] = {
    (smell: Signal, id: Int, numberOfFlights: Int, experience: Experience, hunger: Int) =>
      Bee(id, arg.smellWith(smell), numberOfFlights, experience, hunger)
  }

  def unapply(arg: BufferCell)(implicit config: BeexploreConfig): BeeAccessible[BufferCell] =
    (smell: Signal, id: Int, numberOfFlights: Int, experience: Experience, hunger: Int) =>
      BufferCell(Bee(id, arg.smellWith(smell), numberOfFlights, experience, hunger))

  def unapply(arg: FlowerPatch)(implicit config: BeexploreConfig): BeeAccessible[Bee] =
    (smell: Signal, id: Int, numberOfFlights: Int, experience: Experience, hunger: Int) =>
      Bee(id, arg.smellWith(smell), numberOfFlights, experience, hunger)

}
