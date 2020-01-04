package pl.edu.agh.beexplore.model

import pl.edu.agh.beexplore.model.Bee.{BeeRole, Experience}
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class Bee(
                      smell: SmellArray,
                      numberOfFlights: Int,
                      experience: Experience,
                      hunger: Int,
                      role: BeeRole,
                    ) extends SmellingCell {
  override type Self = Bee

  override def withSmell(smell: SmellArray): Bee = copy(smell = smell)

  def withExperience(experience: Experience): Bee = copy(experience = experience)

}

object Bee {

  sealed trait BeeRole

  case object Scout extends BeeRole

  case object Forager extends BeeRole

  case object None extends BeeRole

  sealed trait Experience

  case object Novice extends Experience

  case object Intermediate extends Experience

  case object Expert extends Experience

  def create(initialSignal: Signal): Bee = new Bee(
    Array.fill(Cell.Size, Cell.Size)(initialSignal),
    numberOfFlights = 0,
    experience = Novice,
    hunger = 0,
    role = None
  )
}
