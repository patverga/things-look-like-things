package co.pemma

import java.io.{FileWriter, BufferedWriter, PrintWriter}

import cc.factorie.app.nlp.load
import cc.factorie.app.nlp.Sentence

import scala.util.matching.Regex

/**
 * Created by pat on 7/9/14.
 */
object SnowBall
{

  def run(line : String, outputLocation : String)
  {
    val queries = parseInputLineToQueries(line)

    val matches = extractSeedOccurances(queries)

    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation)))
    matches.foreach(s => {
      println(s"\n\n ${s.group(0)}")
      writer.println(s"\n\n ${s.group(0)}")
    })
    writer.close()

//    val contexts = extractContextsFromMatches(matches)
  }

  /**
   * input in form a:b,c,d,e where a is a thing and b-e are things
   * or descriptions of what a looks like
   * @param line
   */
  def parseInputLineToQueries(line : String) : Seq[String] =
  {
    val thingAndDescriptions = line.split(":")
    val thing = thingAndDescriptions(0)
    val descriptions = thingAndDescriptions(1).split(",")

    val queries = descriptions.map(d => {
      s"$thing $d"
    })
    queries
  }

  def extractSeedOccurances(queries : Seq[String]) : Set[Regex.Match] =
  {
    // run queries and process results
//    val docs = queries.flatMap(q => GalagoWrapper.runQuery(q)).toSet[String]
//    print("Processing Docs...")
//    val allSentences = docs.flatMap(d => {
//      val doc = load.LoadPlainText.fromString(d).head
//      NLPThings.pipeline.process(doc).sentences
//    })

    print("...Extracting seed relation matches...")
    // extract lines that match the seed relations
    val matchRegex = queries.map(q => {
      val words = q.split(" ", 2)
      s"(?:.*${words(0)}.*${words(1)}.*)|(?:.*${words(1)}.*${words(0)}.*)"
    }).mkString("|")

    print("filtering...")
//    val filteredSentences = allSentences.filter(_.string.matches(matchRegex))

    // extract contexts around matches
    val used = collection.mutable.HashSet[String]()
    val contextRegexString = queries.map(q => {
      val words = q.split(" ", 2)
      var line = words(1)
      if (!used.contains(words(0)))
      {
        line += s"|${words(0)}"
        used.+=(words(0))
      }
      line
    }).mkString("(.*)(", "|", ")")
    val contextRegex = contextRegexString + contextRegexString + "(.*)"
    println(s"\n${matchRegex.toString()} \n${contextRegex.toString()}")

    print("extracting context...")
    // get context matches
//    filteredSentences.flatMap(s => contextRegex.findAllMatchIn(s.string))
    Set()
  }

//  def extractContextsFromMatches(matches : Set[Regex.Match] )
//  {
//    matches.
//  }
}
