package pl.edu.agh.beexplore.utils

import java.awt.Color
import java.io.File

import javax.imageio.ImageIO
import pl.edu.agh.beexplore.config.BeexploreConfig
import pl.edu.agh.beexplore.model.{Beehive, FlowerPatch}
import pl.edu.agh.xinuk.model.{EmptyCell, Grid, Signal}

import scala.util.Try

object MapReader {

  val (red, blue, white) = (new Color(255, 0, 0), new Color(0, 101, 248), new Color(255, 255, 255))

  def read(name: String, emptyGrid: Grid)(implicit config: BeexploreConfig): Try[Grid]={
    val img = ImageIO.read(getClass.getClassLoader.getResource(name))
    val (w, h) = (img.getWidth, img.getHeight)
    Try {
      require(w == h)
      require(w == config.gridSize)
      for{
        x <- 0 until w
        y <- 0 until h
      }{
        new Color(img.getRGB(x, y)) match {
          case color if color == white =>
          case color if similarTo(color, red) =>
            emptyGrid.cells(x)(y) = Beehive.create(Signal.Zero, (x, y))
          case color if similarTo(color, blue) =>
            emptyGrid.cells(x)(y) = FlowerPatch.create(Signal(0.4))
        }
      }
      emptyGrid
    }
  }

  def colorDistance (c: Color, flowerPatchColor: Color): Double = {
    (c.getRed - flowerPatchColor.getRed) * (c.getRed - flowerPatchColor.getRed)
    + (c.getGreen - flowerPatchColor.getGreen) * (c.getGreen - flowerPatchColor.getGreen)
    + (c.getBlue - flowerPatchColor.getBlue) * (c.getBlue - flowerPatchColor.getBlue)
  }

  def similarTo(c: Color, flowerPatchColor: Color) = {
    val distance = colorDistance(c, flowerPatchColor)
    if (distance < 1500) true
    else false
  }
}
