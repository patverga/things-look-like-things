package co.pemma

import cc.factorie.app.nlp.load
import co.pemma.galagos.GalagoWrapper
import co.pemma.relationExtractors.RelationExtractor

/**
 * Created by pat on 7/16/14.
 */
object StupidBullshit
{

  def getAllRelationsForThing(thing : String, outputLocation : String, extractor : RelationExtractor, galago : GalagoWrapper)
  {
    val documents = galago.runQuery(thing, 10000)

    // load the data
    var i = 0
    println("Processing Documents...")
    val sentences = documents.flatMap(document =>
    {
      i += 1
      Utilities.printPercentProgress(i, documents.size)

      val doc = load.LoadPlainText.fromString(document).head
      // doc -> sentences with factorie, keep only sentences that match our pattern
      FactorieFunctions.extractSentences(doc).map(_.string).filter(_.contains(thing))
    })

    i = 0
    println(s"Extracting relations from ${sentences.size} sentences")
    val extractions = sentences.flatMap(sentence =>
    {
      i += 1
      Utilities.printPercentProgress(i, documents.size)
      if (sentence != null && sentence.length > 10)
      {
        extractor.extract(sentence)
      }
      else
        Seq()
    }).filter(x=>{ (x.arg1.contains(thing) | x.arg2.contains(thing)) })

    MainThings.printExtractions(extractions, outputLocation)
  }

}
