package co.pemma.RelationExtractors

/**
 * Created by pat on 7/11/14.
 */
class Extraction(c : Double, a1 : String, r : String, a2 : String, s : String)
{
  val arg1 = a1
  val rel = r
  val arg2 = a2
  val sentence = s
  val confidence = ("%.2f" format c)

  def relation() : String =
  {
    s"$confidence ($arg1; $rel; $arg2)"
  }
}