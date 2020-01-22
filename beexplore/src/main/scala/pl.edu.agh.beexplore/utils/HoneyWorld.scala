package pl.edu.agh.beexplore.utils

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.Beehive
import pl.edu.agh.xinuk.model.Grid

trait HoneyWorld {
  val destroyedPatchesCoords: Iterator[(Int, Int)] = Iterator.empty;
  def create(grid: Grid): Seq[Int]
  def hive() : Beehive
}
