package pl.edu.agh.beexplore.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.SmellingCell

final case class Beehive(smell: SmellArray) extends SmellingCell{
  override type Self = Beehive

  override def withSmell(smell: SmellArray): Beehive = copy(smell=smell)

}
