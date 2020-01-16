package pl.edu.agh.beexplore.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class Bee(
                      smell: SmellArray,
                      experience: Int,
                      hunger: Int,
                    ) extends SmellingCell {
  override type Self = Bee

  override def withSmell(smell: SmellArray): Bee = copy(smell = smell)

}

object Bee {

  def create(initialSignal: Signal): Bee = new Bee(
    Array.fill(Cell.Size, Cell.Size)(initialSignal),
    experience = 0,
    hunger = 0,
  )
}
