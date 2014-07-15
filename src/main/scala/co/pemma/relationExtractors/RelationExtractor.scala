package co.pemma.relationExtractors

import cc.factorie.app.nlp._
import co.pemma.{FactorieFunctions, Utilities}

/**
 * Created by pat on 6/30/14.
 */
abstract class RelationExtractor
{
  val omitArgRegex = "(?:you)|(?:he)|(?:she)|(?:it)|(?:we)|(?:they)|(?:him)|(?:her)|(?:i)|(?:\\W)".r
  val patternRegex = "(?:(?:appear(?:s|ed|ance is)?|look(?:s|ed)?) (?:exactly |almost| pretty much)?(?:the same as|identical to|similar to|like)|(?:resemble(?:s|d)))".r


  def extractRelations(documents : Seq[String]) : Iterable[Extraction] = {
    return extractRelations(documents, "")
  }

  def extractRelations(documents : Seq[String], thing : String) : Iterable[Extraction] =
  {
    // use to filter sentences before extraction for efficiency
    val filterRegex = if (thing == null | thing == "")
      s".*${patternRegex.toString}.*".r
    else
      s"(?:.*$thing.*${patternRegex.toString}.*)|(?:.*${patternRegex.toString}.*$thing.*)".r

    println(filterRegex.toString())
    // load the data
    var i = 0
    println("Processing Documents...")
    val allExtractions = documents.flatMap(document =>
    {
      i += 1
      Utilities.printPercentProgress(i, documents.size)

      val doc = load.LoadPlainText.fromString(document).head
      // doc -> sentences with factorie, keep only sentences that match our pattern
      val sentences = FactorieFunctions.extractSentences(doc).map(_.string).filter(filterRegex.pattern.matcher(_).matches)
      val extractions = sentences.flatMap(sentence =>
      {
        val sentString = sentence.replaceAll("[^\\x00-\\x7F]", "").trim
        if (sentString != "" && sentString != null && sentString.length > 10)
        {
          extract(sentence.toLowerCase())
        }
        else
          Seq()
      })
      extractions
    })
    filter(allExtractions)
  }

  def extract(sentStr : String) : Iterable[Extraction]

  def filter(extractions : Iterable[Extraction]) : Iterable[Extraction] =
  {
    // filter out relations that we dont want
    extractions.filter(x => {
      patternRegex.pattern.matcher(x.rel).matches &&
        !omitArgRegex.pattern.matcher(x.arg1).matches &&
        !omitArgRegex.pattern.matcher(x.arg2).matches
    })
  }
}