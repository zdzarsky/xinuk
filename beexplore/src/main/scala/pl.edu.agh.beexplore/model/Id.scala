package pl.edu.agh.beexplore.model

final case class Id(value: Int) extends AnyVal with Ordered[Id] {
  override def compare(that: Id): Int = Ordering.Int.compare(value, that.value)

  def nextId(current: Id): Id = Id(current.value + 1)
}

object Id {
  final val Start = Id(-1)
}