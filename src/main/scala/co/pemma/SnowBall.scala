package co.pemma

import cc.factorie.app.nlp.load
import cc.factorie.app.nlp.Sentence

import scala.util.matching.Regex

/**
 * Created by pat on 7/9/14.
 */
object SnowBall
{

  def run()
  {
    // turn input file to queries
    val file = this.getClass.getResource("/things-look-like-these")
    val lines = io.Source.fromURL(file).getLines.filterNot(_.startsWith("#")).filter(_ != "").toSeq
    val queries = lines.flatMap(l => parseInputLineToQueries(l))

    val matches = extractSeedOccurances(queries)
    var i = 0
    matches.foreach(s => {
      println(s"\n\n ${s.group(0)}")
      //      i += 1
      //      Utilities.printPercentProgress(i, filteredSentences.size)
    })


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
    val docs = queries.flatMap(q => GalagoWrapper.runQuery(q)).toSet[String]
    val allSentences = docs.flatMap(d => {
      val doc = load.LoadPlainText.fromString(d).head
      NLPThings.pipeline.process(doc).sentences
    })

    print("Extracting seed relation matches from retrieved documents...")
    // extract lines that match the seed relations
    val matchRegex = queries.map(q => {
      val words = q.split(" ", 2)
      s"(?:.*${words(0)}.*${words(1)}.*)"
    }).mkString("|")
    print("filtering...")
    val filteredSentences = allSentences.filter(_.string.matches(matchRegex))

    // extract contexts around matches
    val used = collection.mutable.HashSet[String]()
    val contextRegex = queries.map(q => {
      val words = q.split(" ", 2)
      var line = words(1)
      if (!used.contains(words(0)))
      {
        line += s"|${words(0)}"
        used.+=(words(0))
      }
      line
    }).mkString("(.*)(?:", "|", ")(.*)").r

    print("extracting context...")
    // get context matches
    filteredSentences.flatMap(s => contextRegex.findAllMatchIn(s.string))
  }
}
