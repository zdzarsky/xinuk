package pl.edu.agh.beexplore.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class Beehive(smell: SmellArray, position: (Int, Int), bees: Vector[Bee] = Vector.empty) extends SmellingCell {
  override type Self = Beehive

  override def withSmell(smell: SmellArray): Beehive = copy(smell = smell)



}

object Beehive {

  def create(initialSignal: Signal, position: (Int, Int)): Beehive = new Beehive(
    Array.fill(Cell.Size, Cell.Size)(initialSignal), position
  )
}
