package co.pemma

import scala.util.matching.Regex
/**
 * Created by pat on 6/25/14.
 */
object Regexer
{

  def generateSurfacePatternRegexes(patternListURL: java.net.URL, thing: String): collection.mutable.MutableList[Regex] =
  {
    val patternList = collection.mutable.MutableList[Regex]()

    val anyWordsRegex = ".*?(\\s*(?:\\w+\\s*){0,3}).*?"
//    val anyWordsRegex = ".*?((?:\\w+\\W+){1,4})"
    val thingRegEx = ".*?((?:\\w+\\W+){0,3}\\s*"+thing+"\\s*(?:\\w+\\W+){0,3})"

    io.Source.fromURL(patternListURL).getLines().foreach(line => {
      if (!line.startsWith("#") && line != "") {

        val pattern1 = new Regex(anyWordsRegex + "("+line+")" + thingRegEx)
        val pattern2 = new Regex(thingRegEx + "("+line+")" + anyWordsRegex)
        //        println(pattern1)
        //        println(pattern2)
        patternList += pattern1
        patternList += pattern2
      }
    })
    patternList
  }

  def testRegexMaker()
  {
    val regexList = generateSurfacePatternRegexes(this.getClass.getResource("/patterns"), "actor")

    val testStringList = Seq(
      "actor looks like you",
      "you look like an actor",
      "he looks like the actor looks",
    "does he look like the actor?"
    )

    testStringList.foreach( sentence =>
    {
      val lowerCase = sentence.toLowerCase()
      regexList.foreach( regex =>
      {
        println(regex)
        regex.findAllMatchIn(lowerCase).foreach( matches =>
        {
          println(matches.group(1), matches.group(2), matches.group(3))
        })
      })
    })
  }

}
