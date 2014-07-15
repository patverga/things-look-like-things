package co.pemma.relationExtractors

/**
 * Created by pat on 7/11/14.
 */
class Extraction(c : Double, a1 : String, r : String, a2 : String, s : String)
{
  val arg1 = a1.trim
  val rel = r.trim
  val arg2 = a2.trim
  val sentence = s
  val confidence = ("%.2f" format c)

  def relation() : String =
  {
    s"$confidence ($arg1; $rel; $arg2)"
  }

  override def toString() : String =
  {
    s"$arg1 $rel $arg2"
  }
}