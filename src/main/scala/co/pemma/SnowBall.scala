package co.pemma

import cc.factorie.app.nlp.load
import cc.factorie.app.nlp.Sentence

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

    val filteredSentences = extractSeedOccurances(queries)
    var i = 0
    filteredSentences.foreach(s => {
      println(s)
      i += 1
      Utilities.printPercentProgress(i, filteredSentences.size)
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

  def extractSeedOccurances(queries : Seq[String]) : Set[Sentence] =
  {
    // run queries and process results
    val docs = queries.flatMap(q => GalagoWrapper.getDocumentsForQueryTerms(q)).toSet[String]
    val allSentences = docs.flatMap(d => {
      val doc = load.LoadPlainText.fromString(d).head
      NLPThings.pipeline.process(doc).sentences
    })

    // extract lines that match the seed relations
    val regex = queries.flatMap(q => {
      val words = q.split(" ", 2)
      s"(?:.*${words(0)}.*${words(1)}.*)"
    }).mkString("|")

    // extract relations from docs
   allSentences.filter(_.string.matches(regex))
  }


}
