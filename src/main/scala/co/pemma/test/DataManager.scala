package co.pemma.test

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import cc.factorie.app.nlp.load
import cc.factorie.app.nlp.Sentence
import co.pemma.FactorieFunctions
import co.pemma.Utilities
import co.pemma.galagos.ClueWebQuery

import scala.collection.GenSeq

/**
 * Created by pv on 7/18/14.
 */
object DataManager
{

  def main(args: Array[String])
  {
    exportSentences(args(0).toLowerCase())
  }

  def exportSentences(thing : String)
  {
    val outputLocation = s"data/${thing}.data"

    // get documents from galago
    val galago = new ClueWebQuery
    val documents = galago.runBatchQueries(Seq(s"$thing looks like", s"$thing appears like"), 5000)

    // convert documents to sentences
    var i = 0
    val sentences = documents.par.flatMap(document => {
      i += 1
      Utilities.printPercentProgress(i, documents.size)
      // doc -> sentences with factorie
      FactorieFunctions.extractSentences(load.LoadPlainText.fromString(document).head)
        // filter sentences that dont contain the thing
        .filter(_.contains(thing))
    })

//        val sentences = galago.retrieveMatchingSentences(Seq(s"$thing looks like", s"$thing appears like"), thing, 5000)

    // export the sentences
    printSentences(sentences, outputLocation)
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

  def readInSentences(dataLocation :String)
  {
    val source = scala.io.Source.fromFile(dataLocation)
    val lines = source.mkString
    source.close()
  }

}
