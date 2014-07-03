package co.pemma

import cc.factorie.app.nlp.{Token, load, Sentence}
import java.io.{FileWriter, BufferedWriter, PrintWriter}

import scala.util.matching.Regex

/**
 * Created by pat on 6/25/14.
 */
class Regexer(thing: String)
{

  val patternUrl = this.getClass.getResource("/patterns")
  val regex = generateSurfacePatternRegexes(thing)

  def extractRegexFromSentences(sentences : Iterable[Sentence], thing : String, outputLocation : String)
  {
    println("Looking for things that look like " + thing)
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation, true)))
    sentences.foreach(sentence =>
    {
      val matches = regex.findAllMatchIn(sentence.string)
      matches.foreach(m =>
      {
        println(s"${m.group(0)}")
        writer.println(s"${m.group(0)} \n")
      })
    })
    writer.close()
  }

  def generateSurfacePatternRegexes(thing: String): Regex =
  {
    val anyWordsRegex = ".*"
    val thingRegEx = s"(?:^$thing |(?:.* $thing\\W.*)|(?:.* $thing$$))"

    io.Source.fromURL(patternUrl).getLines.filter(!_.startsWith("#")).filter(_ != "")
      .map(pattern => s"($anyWordsRegex$pattern$thingRegEx)|($thingRegEx$pattern$anyWordsRegex)")
      .mkString("|").r
  }

  def extractRelationForArgs(arg1 : String, arg2 : String, sentence : String)
    : (collection.mutable.MutableList[String], collection.mutable.MutableList[String],
        collection.mutable.MutableList[String]) =
  {
    val regexList = collection.mutable.MutableList[Regex]()
    regexList += s"(.*)$arg1(.*)$arg2(.*)".r
    regexList += s"(.*)$arg2(.*)$arg1(.*)".r

    val left = collection.mutable.MutableList[String]()
    val center = collection.mutable.MutableList[String]()
    val right = collection.mutable.MutableList[String]()

    if ( sentence.matches(s"$arg1(?:\\s*\\S+\\s*){0,10}$arg2") ||
      sentence.matches(s"$arg2(?:\\s*\\S+\\s*){0,10}$arg1") )
    {
      regexList.mkString("|").r.findAllMatchIn(sentence).foreach(m =>
      {
        left += m.group(1)
        center += m.group(2)
        right += m.group(3)
        println(m.group(0))
      })
    }
    (left, center, right)
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
      regex.findAllMatchIn(lowerCase).foreach( matches =>
        {
          println(matches.group(0))
        })
    })
  }

  def main(args: Array[String])
  {
    //    testRegexMaker()

    extractRelationForArgs("cat", "dog", "a cat is not a dog.")
  }

}
