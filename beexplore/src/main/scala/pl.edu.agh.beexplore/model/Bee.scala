package pl.edu.agh.beexplore.model

import pl.edu.agh.beexplore.model.Bee.BeeRole
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.SmellingCell

final case class Bee(
                    smell: SmellArray,
                    experience: Int,
                    hunger: Int,
                    role: BeeRole,
                    ) extends SmellingCell{
  override type Self = Bee

  override def withSmell(smell: SmellArray): Bee = copy(smell = smell)

}

object Bee {
  sealed trait BeeRole
  case object Scout extends BeeRole
  case object Forager extends BeeRole
  case object None extends BeeRole
}
