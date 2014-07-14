package co.pemma

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import co.pemma.RelationExtractors._
import cc.factorie.app.nlp._
import cc.factorie.util.CmdOptions

object MainThings
{
  def exportRelationsByThing(thing : String, outputLocation : String, extractor : RelationExtractor)
  {
    val documents = GalagoWrapper.runQuery(s"$thing looks like", 10000)
    // filter relations that do not involve the 'thing'
    val extractions = extractor.extractRelations(documents).filter(x => {
      (x.arg1.contains(thing) || x.arg2.contains(thing))
    })
    printExtractions(extractions, outputLocation)
  }

  def exportRelationsByPattern(query : String, outputLocation : String, extractor : RelationExtractor)
  {
    val documents = GalagoWrapper.runQuery(query, 5000)
    val extractions = extractor.extractRelations(documents)
    printExtractions(extractions, outputLocation)
  }

  def printExtractions(extractions : Iterable[Extraction], outputLocation : String)
  {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation)))
    extractions.foreach(extract =>
    {
      println(s"\n ${extract.sentence} \n ${extract.relation}")
      writer.println(s"\n ${extract.sentence} \n ${extract.relation}")
    })
    writer.close()
  }


  /**
   * given a thing, find all 'looks like' relations involving that thing
   * @param thing
   * @param output
   */
  def findThingsThatLookLikeThisThingFromGalago(thing : String, output : String)
  {
    val regexerObject = new Regexer(thing, ".*")

    val documents = regexerObject.patternList.flatMap(pattern =>
    {
      GalagoWrapper.runQuery(s"${pattern.replaceAll("\\?", "")} $thing")
    })

    // load the data
    var i = 0
    val docSet = documents.toSet[String]
    println("Processing Documents...")
    docSet.foreach(document =>
    {
      val doc = load.LoadPlainText.fromString(document).head
      val sentences = FactorieFunctions.extractSentences(doc)
      regexerObject.extractRegexFromSentences(sentences, thing, output)
      i += 1
      Utilities.printPercentProgress(i, docSet.size)
    })
  }

  class ProcessSlotFillingCorpusOpts extends CmdOptions {
    val thing = new CmdOption("thing", "", "STRING...", "Takes as input one string and finds things that look like it.")
    val pattern = new CmdOption("pattern", "", "STRING...",  "Uses Ollie to extract relations for our seed patterns from a galago search of those patterns.")
    val snowBall = new CmdOption("snowball", "", "STRING...",  "Google : 'snowball urban dictionary'. You're welcome.")
    val extractor = new CmdOption("extractor", "", "STRING...",  "Choose which openIE system to use. reverb, ollie, or clauseie")
  }

  def main(args: Array[String])
  {
    println(s"Input Args : ${args.mkString(" ")}")

    val opts = new ProcessSlotFillingCorpusOpts
    opts.parse(args)

    val extractorType = if (opts.extractor.wasInvoked)
      opts.extractor.value.toLowerCase
    else
      "clauseie"
    val output = s"results/${extractorType}/"
    val extractor = extractorType match{
      case "reverb" => new ReverbExtractor
      case "ollie" => new OllieExtractor
      case "clauseie" => new ClauseIEExtractor
    }

    if (opts.thing.wasInvoked)
    {
      val thing = opts.thing.value.toLowerCase
      exportRelationsByThing(thing, s"${output}thing/$thing.result", extractor)
    }
    else if (opts.pattern.wasInvoked) {
      val query = opts.pattern.value.toLowerCase.replaceAll("\\?","")
      if (!query.startsWith("#") && query != "") {
        exportRelationsByPattern(query, s"${output}pattern/${query.replaceAll("\\s+","-")}.result", extractor)
      }
    }

    println("Done.")
  }
}
