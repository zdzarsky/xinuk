package pl.edu.agh.beexplore.utils

import java.awt.Color

object DistanceUtils {
  def colorDistance(c: Color, flowerPatchColor: Color): Double = {
    squaresSum(List(
      c.getRed - flowerPatchColor.getRed,
      c.getGreen - flowerPatchColor.getGreen,
      c.getBlue - flowerPatchColor.getBlue))
  }

  def similarTo(c: Color, flowerPatchColor: Color) = {
    colorDistance(c, flowerPatchColor) < 1500
  }

  private def squaresSum(xs: List[Double]): Double = {
    xs.foldLeft(0.0) { (a, x) => a + x * x }
  }
}
