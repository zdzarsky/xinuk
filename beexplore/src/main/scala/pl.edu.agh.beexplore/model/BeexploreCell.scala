package pl.edu.agh.beexplore.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, SmellingCell}

final case class BeexploreCell(smell: SmellArray, bees: Vector[Bee], flowerPatch: Id) extends SmellingCell {
  override type Self = BeexploreCell

  override def withSmell(smell: SmellArray): BeexploreCell = copy(smell = smell)
}

object BeexploreCell {
  def create(bees: Vector[Bee] = Vector.empty): BeexploreCell =
    BeexploreCell(Cell.emptySignal, bees, Id.Start)
}
