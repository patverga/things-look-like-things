package co.pemma

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import co.pemma.galagos.{ClueWebQuery, WikipediaQuery, GalagoWrapper}
import co.pemma.relationExtractors._
import cc.factorie.util.CmdOptions
import edu.mit.jwi.item.Pointer

import scala.collection.GenSeq

object MainThings
{
  def exportRelationsByThing(thing : String, outputLocation : String, extractor : RelationExtractor, galago : GalagoWrapper)
  {
    val documents = galago.runQuery(s"$thing looks like", 10000)
    // filter relations that do not involve the 'thing'
    val extractions = extractor.extractRelations(documents, thing).filter(x => {
      (x.arg1.contains(thing) || x.arg2.contains(thing))
    })
    printExtractions(extractions, outputLocation)
  }

  def exportRelationsByThingAndSynonyms(thing : String, outputLocation : String, extractor : RelationExtractor, galago : GalagoWrapper)
  {
    val hypos = JWIWordNetWrap.getHypnym(thing, Pointer.HYPONYM) ++ JWIWordNetWrap.getHypnym(thing, Pointer.HYPERNYM) ++ thing
    val thingRegex = hypos.mkString("(?:","|",")")
    val queries = hypos.map(h => s"$h looks like")
    val documents = galago.runBatchQueries(queries)

    // filter relations that do not involve the 'thing'
    val extractions = extractor.extractRelations(documents, thingRegex).filter(x => {
      (x.arg1.matches(thingRegex) || x.arg2.matches(thingRegex))
    })
    printExtractions(extractions, outputLocation)
  }

  def exportRelationsByPattern(query : String, outputLocation : String, extractor : RelationExtractor, galago : GalagoWrapper)
  {
    val documents = galago.runQuery(query, 10000)
    val extractions = extractor.extractRelations(documents)
    printExtractions(extractions, outputLocation)
  }

  def printExtractions(extractions : GenSeq[Extraction], outputLocation : String)
  {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation)))
    extractions.foreach(extract =>
    {
      println(s"\n ${extract.sentence} \n ${extract.relation}")
      writer.println(s"\n ${extract.sentence} \n ${extract.relation}")
    })
    writer.close()
  }





  class ThingsLookeLikeThingsCmdParser extends CmdOptions {
    val thing = new CmdOption("thing", "", "STRING...", "Takes as input one string and finds things that look like it.")
    val syns = new CmdOption("syns", "", "STRING...", "Takes as input one string and finds things that look like it and its synonyms.")
    val pattern = new CmdOption("pattern", "", "STRING...",  "Uses Ollie to extract relations for our seed patterns from a galago search of those patterns.")
    val snowBall = new CmdOption("snowball", "", "STRING...",  "Google : 'snowball urban dictionary'. You're welcome.")
    val extractor = new CmdOption("extractor", "", "STRING...",  "Choose which openIE system to use: reverb, ollie, or clauseie (Default = ClauseIE)")
    val data = new CmdOption("data", "", "STRING...",  "Choose which index to use: clueweb or wikipedia. (Default = clueweb)")
  }

  def main(args: Array[String])
  {
    println(s"Input Args : ${args.mkString(" ")}")

    val opts = new ThingsLookeLikeThingsCmdParser
    opts.parse(args)

    // choose data set galago index
    val dataset = if (opts.data.wasInvoked)
      opts.data.value.toLowerCase
    else
      "clueweb"
    val galago = dataset match {
        case "wikipedia" => new WikipediaQuery
        case "clueweb" => new ClueWebQuery
        case _ => new ClueWebQuery
      }

    // choose extractor
    val extractorType = if (opts.extractor.wasInvoked)
      opts.extractor.value.toLowerCase
    else
      "clauseie"
    val extractor = extractorType match{
      case "reverb" => new ReverbExtractor
      case "ollie" => new OllieExtractor
      case "clauseie" => new ClauseIEExtractor
      case _ => new ClauseIEExtractor
    }

    // set output
    val output = s"results/${dataset}/${extractorType}/"

    // run the chosen method
    if (opts.thing.wasInvoked)
    {
      val thing = opts.thing.value.toLowerCase
      exportRelationsByThing(thing, s"${output}thing/$thing.result", extractor, galago)
    }
    if (opts.syns.wasInvoked)
    {
      val thing = opts.syns.value.toLowerCase
      exportRelationsByThingAndSynonyms(thing, s"${output}synonyms/$thing.result", extractor, galago)
    }
    else if (opts.pattern.wasInvoked) {
      val query = opts.pattern.value.toLowerCase.replaceAll("\\?","")
      if (!query.startsWith("#") && query != "") {
        exportRelationsByPattern(query, s"${output}pattern/${query.replaceAll("\\s+","-")}.result", extractor, galago)
      }
    }

    println("Done.")
  }
}
