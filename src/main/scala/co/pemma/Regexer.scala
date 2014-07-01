package co.pemma

import cc.factorie.app.nlp.Sentence

import scala.util.matching.Regex
/**
 * Created by pat on 6/25/14.
 */
object Regexer
{

  def extractRegexFromSentences(sentences : Iterable[Sentence], thing : String, output : String)
  {
    println("Looking for things that look like " + thing)
    sentences.foreach(sentence =>
    {
      val matches = extractRegexFromString(sentence.string, thing)
      matches.foreach(m =>
      {
        println(s"${m.group(1)} - ${m.group(2)} - ${m.group(3)}")
        //        println(matches.group(0))
      })
    })
  }

  def extractRegexFromString(documentString : String, thing : String) : collection.mutable.MutableList[Regex.Match] =
  {
    // set file defining patterns
    val patternUrl = this.getClass.getResource("/patterns")
    // convert patterns to regex
    val regexList = generateSurfacePatternRegexes(patternUrl, thing.toLowerCase())

    val lowerSentence = documentString.toLowerCase()
    val matches =  collection.mutable.MutableList[Regex.Match]()
    regexList.foreach( regex =>
    {
      matches ++= regex.findAllMatchIn(lowerSentence)
    })
    matches
  }

  def generateSurfacePatternRegexes(patternListURL: java.net.URL, thing: String): collection.mutable.MutableList[Regex] =
  {
    val patternList = collection.mutable.MutableList[Regex]()

    val anyWordsRegex = "((?:\\s*\\S+\\s*){1,4})"
    val thingRegEx = "((?:\\s*\\S+\\s*){0,4}"+thing+"(?:\\s*\\S+\\s*){0,4})"

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
