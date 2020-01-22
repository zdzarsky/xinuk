package pl.edu.agh.beexplore.model

import pl.edu.agh.beexplore.model.Bee.{Experience}
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class Bee(
                      id: Int,
                      smell: SmellArray,
                      numberOfFlights: Int,
                      experience: Experience,
                      hunger: Int,
                    ) extends SmellingCell {
  override type Self = Bee

  override def withSmell(smell: SmellArray): Bee = copy(smell = smell)

  def withExperience(experience: Experience): Bee = copy(experience = experience)

}

object Bee {

  sealed trait Experience

  case object Novice extends Experience

  case object Intermediate extends Experience

  case object Experienced extends Experience

  case object Expert extends Experience

  def create(id: Int, initialSignal: Signal): Bee = new Bee(
    id,
    Array.fill(Cell.Size, Cell.Size)(initialSignal),
    numberOfFlights = 0,
    experience = Novice,
    hunger = 0,
  )
}
