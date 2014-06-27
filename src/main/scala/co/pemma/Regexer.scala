package co.pemma

import scala.util.matching.Regex
/**
 * Created by pat on 6/25/14.
 */
object Regexer
{

  def extractRegexFromString(documentString : Iterable[cc.factorie.app.nlp.Sentence], thing : String)
  {
    // set file defining patterns
    val patternUrl = this.getClass.getResource("/patterns")
    // convert patterns to regex
    val regexList = generateSurfacePatternRegexes(patternUrl, thing.toLowerCase())

    println("Looking for things that look like " + thing)
    documentString.foreach( sentence =>
    {
      val lowerSentence = sentence.string.toLowerCase
      //    val lowerSentence = documentString.toLowerCase()
      regexList.foreach( regex =>
      {
        regex.findAllMatchIn(lowerSentence).foreach( matches =>
        {
          println(regex)
          println(matches.group(1), matches.group(2), matches.group(3))
          println(matches.group(0))
        })
      })
    })
  }

  def generateSurfacePatternRegexes(patternListURL: java.net.URL, thing: String): collection.mutable.MutableList[Regex] =
  {
    val patternList = collection.mutable.MutableList[Regex]()

    val anyWordsRegex = "([\\s*\\S+\\s*]*{1,4})"
    val thingRegEx = "([\\s*\\S+\\s*]*{0,4}"+thing+"[\\s*\\S+\\s*]*{0,4})"

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
    "does he look like the actor?",
    "he looks like a factor",
    "actor looks like an actor"
    )

    testStringList.foreach( sentence =>
    {
      val lowerCase = sentence.toLowerCase()
      regexList.foreach( regex =>
      {
        regex.findAllMatchIn(lowerCase).foreach( matches =>
        {
          println(regex)
          println(matches.group(1), matches.group(2), matches.group(3))
        })
      })
    })
  }

}
