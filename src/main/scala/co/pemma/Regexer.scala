package co.pemma

import cc.factorie.app.nlp.{Token, load, Sentence}
import java.io.{FileWriter, BufferedWriter, PrintWriter}

import scala.util.matching.Regex

/**
 * Created by pat on 6/25/14.
 */
class Regexer(thing1: String, thing2: String)
{
  val patternUrl = this.getClass.getResource("/patterns")
  val patternList = io.Source.fromURL(patternUrl).getLines.filterNot(_.startsWith("#")).filter(_ != "").toSeq
  val patternRegex = generateSurfacePatternRegexes(thing1)

  val context1Regex =  s"(?:(.*)($thing1)(\\s*(?:\\S+\\s*){0,10})($thing2)(.*))".r
  val context2Regex =  s"(?:(.*)($thing2)(\\s*(?:\\S+\\s*){0,10})($thing1)(.*))".r

  def extractRegexFromSentences(sentences : Iterable[Sentence], thing : String, outputLocation : String)
  {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation, true)))
    sentences.foreach(sentence =>
    {
      val matches = patternRegex.findAllMatchIn(sentence.string)
      matches.foreach(m =>
      {
        println(s"${m.group(0)} \n")
        writer.println(s"${m.group(0)} \n")
      })
    })
    writer.close()
  }

  def generateSurfacePatternRegexes(thing: String): Regex =
  {
    val anyWordsRegex = ".*"
    val thingRegEx = s"(?:^$thing |(?:.* $thing\\W.*)|(?:.* $thing$$))"

    patternList.map(pattern => s"($anyWordsRegex$pattern$thingRegEx)|($thingRegEx$pattern$anyWordsRegex)").mkString("|").r
  }

  def extractContextsForRelation(sentence : String) : Iterator[Regex.Match] =
  {
    val m1 = context1Regex.findAllMatchIn(sentence)
    val m2 = context2Regex.findAllMatchIn(sentence)

    return (m1 ++ m2)
  }




  def testRegexMaker()
  {
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
      patternRegex.findAllMatchIn(lowerCase).foreach( matches =>
      {
        println(matches.group(0))
      })
    })
  }

  def testContextExtractor()
  {
    val arg1 = "cat"
    val arg2 = "dog"

    val testStringList = Seq(
      "this cat is like a dog",
      "this dog is like a cat",
      "cat 1 2 3 4 5 6 7 8 9 10 11 12 dog",
      "no cat no dog",
      "cat dog"
    )

    testStringList.foreach( sentence =>
    {
      val lowerCase = sentence.toLowerCase()
      println(sentence)
      extractContextsForRelation(lowerCase).foreach( matches =>
      {
        println(s"\t ${matches.group(1)} : ${matches.group(2)} : ${matches.group(3)}")
      })
    })
  }
}
