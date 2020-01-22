package pl.edu.agh.beexplore.utils
import pl.edu.agh.beexplore.model.Beehive
import pl.edu.agh.xinuk.model.{Grid, Signal}

class MapWorld extends HoneyWorld {
  override def create(grid: Grid): Seq[Int] = {Seq.empty}

  override def hive(): Beehive = null
}
