package pl.edu.agh.beexplore.model.accesibles

import javax.management.relation.Role
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.Bee
import pl.edu.agh.beexplore.model.Bee.{BeeRole, Scout}
import pl.edu.agh.xinuk.model.{EmptyCell, GridPart}

trait BeeAccessible[+T <: GridPart] {
  def withBee(): T
}

object BeeAccessible {
  def unapply(arg: EmptyCell, exp: Int, hunger: Int, role: BeeRole)(implicit config: BeexploreConfig): BeeAccessible[Bee] = {
    () => Bee(arg.smellWith(config.beeSignalInitial), exp, hunger, role)
  }
}
