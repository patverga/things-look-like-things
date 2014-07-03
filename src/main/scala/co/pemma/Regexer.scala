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

    io.Source.fromURL(patternUrl).getLines.filter(!_.startsWith("#")).filter(_ != "")
      .map(pattern => s"($anyWordsRegex$pattern$thingRegEx)|($thingRegEx$pattern$anyWordsRegex)")
      .mkString("|").r
  }

  def extractContextsForRelation(arg1 : String, arg2 : String, sentence : String) :
  Iterator[Regex.Match] =
  {
    // args must be within 10 words of eachother
    val context1Regex =  s"(?:(.*)$arg1(\\s*(?:\\S+\\s*){0,10})$arg2(.*))".r
    val context2Regex =  s"(?:(.*)$arg2(\\s*(?:\\S+\\s*){0,10})$arg1(.*))".r

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
      regex.findAllMatchIn(lowerCase).foreach( matches =>
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
      extractContextsForRelation(arg1, arg2, lowerCase).foreach( matches =>
      {
        println(s"\t ${matches.group(1)} : ${matches.group(2)} : ${matches.group(3)}")
      })
    })
  }
}
