package co.pemma.test

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import cc.factorie.app.nlp.{Sentence, load}
import co.pemma.galagos.ClueWebQuery
import co.pemma.{FactorieFunctions, Utilities}

import scala.collection.GenSeq

/**
 * Created by pv on 7/18/14.
 */
object ExportSentences
{

  def main(args: Array[String])
  {
    val thing = "whippet"
    val outputLocation = s"data/${thing}.data"

    // get documents from galago
    val galago = new ClueWebQuery
    val documents = galago.runBatchQueries(Seq(s"$thing looks like", s"$thing appears like"), 5000)

    // convert documents to sentences
    var i = 0
    val sentences = documents.flatMap(document => {
      i += 1
      Utilities.printPercentProgress(i, documents.size)
      // doc -> sentences with factorie
      FactorieFunctions.extractSentences(load.LoadPlainText.fromString(document).head)
    })

    // filter sentences that dont contain the thing
    val filteredSenteces = sentences.filter(_.contains(thing))

    // export the sentences
    printSentences(filteredSenteces, outputLocation)
  }

  def printSentences(sentences : GenSeq[Sentence], outputLocation : String)
  {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation)))
    sentences.foreach(sentence =>
    {
      val stringSent = sentence.string
      println(s"\n ${stringSent}")
      writer.println(s"\n ${stringSent}")
    })
    writer.close()
  }

}
