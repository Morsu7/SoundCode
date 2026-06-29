package soundcode.engine

import soundcode.domain.*

import scala.annotation.tailrec

object CycleCalculator:
  @tailrec
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private def lcm(a: Int, b: Int): Int = if (a == 0 || b == 0) 1 else (a * b).abs / gcd(a, b)

  def lengthOf(element: Element): Int = element match
    case RecursivePattern.SubPattern(sub) => lengthOf(sub)
    case RecursivePattern.AlternationPattern(alt) =>
      // alt è List[Layer], calcoliamo le scelte (layer.size) per ogni traccia parallela
      alt.map { layer =>
        val choices = layer.size
        val inner = layer.map(lengthOf).foldLeft(1)(lcm)
        choices * inner
      }.foldLeft(1)(lcm)
    case _ => 1

  def lengthOf(pattern: Pattern): Int =
    pattern.map(layer => layer.map(lengthOf).foldLeft(1)(lcm)).foldLeft(1)(lcm)

  def lengthOf(track: Track): Int =
    val baseLen = lengthOf(track.base)
    val extLens = track.extensions.map(lengthOf)
    (baseLen :: extLens).reduce(lcm)
