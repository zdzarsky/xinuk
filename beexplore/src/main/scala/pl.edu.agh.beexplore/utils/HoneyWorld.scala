package pl.edu.agh.beexplore.utils

import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.Beehive
import pl.edu.agh.xinuk.model.Grid

import scala.collection.mutable

trait HoneyWorld {
  val destroyedPatchesCoords: mutable.ListBuffer[(Int, Int)] = mutable.ListBuffer.empty;
  def create(grid: Grid): Unit
  def hive() : Beehive
  def updateHive(hive: Beehive): Unit
  def beeIds(): Seq[Int]
}
