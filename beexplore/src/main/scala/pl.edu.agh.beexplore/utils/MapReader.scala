package pl.edu.agh.beexplore.utils

import java.awt.Color
import java.io.File

import javax.imageio.ImageIO
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{Bee, Beehive, FlowerPatch}
import pl.edu.agh.xinuk.model.{EmptyCell, Grid, Signal}

import scala.util.Try

object MapReader {

  val (red, blue, white) = (new Color(255, 0, 0), new Color(0, 101, 248), new Color(255, 255, 255))

  def read(name: String, emptyGrid: Grid)(implicit config: BeexploreConfig): Try[Grid] = {
    val img = ImageIO.read(getClass.getClassLoader.getResource(name))
    val (w, h) = (img.getWidth, img.getHeight)
    Try {
      require(w == h)
      require(w == config.gridSize)
      for {
        x <- emptyGrid.cells.indices
        y <- emptyGrid.cells.indices
        if GridUtils.notInBufferZone(x, y)
      } {
        new Color(img.getRGB(x, y)) match {
          case color if color == white =>
          case color if similarTo(color, red) =>
            emptyGrid.cells(x)(y) = Beehive.create(Signal.Zero, (x, y), Vector.empty)
          case color if similarTo(color, blue) =>
            emptyGrid.cells(x)(y) = FlowerPatch.create(Signal(0.4))
          case _ =>
            emptyGrid.cells(x)(y) = EmptyCell.Instance
        }
      }
      emptyGrid
    }
  }

  def colorDistance(c: Color, c2: Color): Double = {
    Math.sqrt((c.getRed - c2.getRed) * (c.getRed - c2.getRed)
      + (c.getGreen - c2.getGreen) * (c.getGreen - c2.getGreen)
      + (c.getBlue - c2.getBlue) * (c.getBlue - c2.getBlue))
  }

  def similarTo(c: Color, flowerPatchColor: Color) = {
    val distance = colorDistance(c, flowerPatchColor)
    distance < 200
  }
}
