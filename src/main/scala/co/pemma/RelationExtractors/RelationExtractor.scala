package co.pemma.RelationExtractors

import cc.factorie.app.nlp._
import co.pemma.{FactorieFunctions, Utilities}

/**
 * Created by pat on 6/30/14.
 */
abstract class RelationExtractor
{
  val omitArgRegex = "(?:you)|(?:he)|(?:she)|(?:it)|(?:we)|(?:they)|(?:him)|(?:her)|(?:i)|(?:\\W)".r
  val patternRegex = "(?:appear(?:ance is)?|look(?:s|ed)?) (?:exactly |almost| pretty much)?(?:the same as|identical to|similar to|like)".r

  /**
   *
   * @param documents
   * @return Iterable[(confidence, relation, full sentence)]
   */
  def extractRelations(documents : Seq[String]) : Iterable[Extraction] =
  {
    // load the data
    var i = 0
    println("Processing Documents...")
    val allExtractions = documents.flatMap(document =>
    {
      i += 1
      Utilities.printPercentProgress(i, documents.size)

      val doc = load.LoadPlainText.fromString(document).head
      val sentences = FactorieFunctions.extractSentences(doc)
      val extractions = sentences.flatMap(sentence =>
      {
        val sentString = sentence.string.replaceAll("[^\\x00-\\x7F]", "").trim
        if (sentString != "" && sentString != null && sentString.length > 10)
        {
          extract(sentence.string.toLowerCase())
        }
        else
          Seq()
      })
      extractions
    })
    // filter out relations that we dont want
    allExtractions.filter(x => {
      patternRegex.pattern.matcher(x.rel).matches &&
        !omitArgRegex.pattern.matcher(x.arg1).matches &&
        !omitArgRegex.pattern.matcher(x.arg2).matches
    })
  }

  def extract(sentStr : String) : Iterable[Extraction] =
  {
      Seq()
  }


}