package pl.edu.agh.beexplore.model

final case class BeeAction(currentCellResult: Iterator[Bee], moves: Iterator[((Int, Int), Bee)] = Iterator.empty)
