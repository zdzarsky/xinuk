package pl.edu.agh.beexplore.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.SmellingCell

final case class FlowerPatch(smell: SmellArray) extends SmellingCell{
  override type Self = FlowerPatch

  override def withSmell(smell: SmellArray): FlowerPatch = copy(smell = smell)
}
