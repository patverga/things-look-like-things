package co.pemma.relationExtractors

import cc.factorie.app.nlp._
import co.pemma.{FactorieFunctions, Utilities}

import scala.collection.GenSeq

/**
 * Created by pat on 6/30/14.
 */
abstract class RelationExtractor
{
  val omitArgRegex = "(?:you)|(?:he)|(?:she)|(?:it)|(?:we)|(?:they)|(?:him)|(?:her)|(?:i)|(?:\\W)".r
  val patternRegex = "(?:(?:appear(?:s|ed|ance is)?|look(?:s|ed)?) (?:exactly |almost| pretty much)?(?:the same as|identical to|similar to|like)|(?:resemble(?:s|d)))".r


  def extractRelations(documents : GenSeq[String]) : GenSeq[Extraction] = {
    return extractRelations(documents, "")
  }

  def extractRelations(documents : GenSeq[String], thingRegex : String) : GenSeq[Extraction] =
  {
    // use to filter sentences before extraction for efficiency
    val filterRegex = if (thingRegex == null | thingRegex == "")
      s".*${patternRegex.toString}.*".r
    else
      s"(?:.*$thingRegex.*${patternRegex.toString}.*)|(?:.*${patternRegex.toString}.*$thingRegex.*)".r

    println(filterRegex.toString())
    // load the data
    var i = 0
    println("Processing Documents...")
    val sentences = documents.flatMap(document =>
    {
      i += 1
      Utilities.printPercentProgress(i, documents.size)

      val doc = load.LoadPlainText.fromString(document).head
      // doc -> sentences with factorie, keep only sentences that match our pattern
      FactorieFunctions.extractSentences(doc).map(_.string).filter(filterRegex.pattern.matcher(_).matches)
    })

    i = 0
    println(s"Extracting relations from ${sentences.size} sentences")
    val extractions = sentences.flatMap(sentence =>
    {
      i += 1
      Utilities.printPercentProgress(i, documents.size)
      if (sentence != null && sentence.length > 10)
      {
        extract(sentence)
      }
      else
        Seq()
    })
    filter(extractions)
  }

  def extract(sentStr : String) : Iterable[Extraction]

  def filter(extractions : GenSeq[Extraction]) : GenSeq[Extraction] =
  {
    // filter out relations that we dont want
    extractions.filter(x => {
      patternRegex.pattern.matcher(x.rel).matches &&
        !omitArgRegex.pattern.matcher(x.arg1).matches &&
        !omitArgRegex.pattern.matcher(x.arg2).matches
    })
  }
}