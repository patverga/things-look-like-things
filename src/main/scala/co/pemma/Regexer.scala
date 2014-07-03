package co.pemma

import cc.factorie.app.nlp.{Token, load, Sentence}
import java.io.{FileWriter, BufferedWriter, PrintWriter}

import scala.util.matching.Regex

/**
 * Created by pat on 6/25/14.
 */
object Regexer
{

  def extractRegexFromSentences(sentences : Iterable[Sentence], thing : String, outputLocation : String)
  {
    println("Looking for things that look like " + thing)
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation, true)))
    sentences.foreach(sentence =>
    {
      val matches = extractRegexFromString(sentence.string, thing)
      matches.foreach(m =>
      {
        println(s"${m.group(1)} - ${m.group(2)} - ${m.group(3)}")
        writer.println(m.group(0))
      })
    })
    writer.close()
  }

  def extractRegexFromString(documentString : String, thing : String) : Iterator[scala.util.matching.Regex.Match] =
  {
    // set file defining patterns
    val patternUrl = this.getClass.getResource("/patterns")
    // convert patterns to regex
    val regexList = generateSurfacePatternRegexes(patternUrl, thing.toLowerCase())

    regexList.mkString("|").r.findAllMatchIn(documentString.toLowerCase())
  }

  def generateSurfacePatternRegexes(patternListURL: java.net.URL, thing: String): collection.mutable.MutableList[Regex] =
  {
    val patternList = collection.mutable.MutableList[Regex]()

    val anyWordsRegex = "((?:\\s*\\S+\\s*){1,4})"
    val thingRegEx = "((?:\\s*\\S+\\s*){0,4}"+thing+"(?:\\s*\\S+\\s*){0,4})"

    io.Source.fromURL(patternListURL).getLines().foreach(line => {
      if (!line.startsWith("#") && line != "") {

        val pattern1 = (anyWordsRegex + s"( $line )" + thingRegEx).r
        val pattern2 = (thingRegEx + s"( $line )" + anyWordsRegex).r
        //        println(pattern1)
        //        println(pattern2)
        patternList += pattern1
        patternList += pattern2
      }
    })
    patternList
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

  def main(args: Array[String])
  {
    //    testRegexMaker()

    extractRelationForArgs("cat", "dog", "a cat is not a dog.")
  }

}
