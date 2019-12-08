package pl.edu.agh.beexplore.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class Beehive(smell: SmellArray) extends SmellingCell{
  override type Self = Beehive

  override def withSmell(smell: SmellArray): Beehive = copy(smell=smell)

}

object Beehive {
  def create(initialSignal: Signal): Beehive = new Beehive(
    Array.fill(Cell.Size, Cell.Size)(initialSignal),
  )
}
