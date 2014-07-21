package co.pemma.test

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import cc.factorie.app.nlp.load
import co.pemma.galagos.ClueWebQuery
import co.pemma.Regexer
import co.pemma.relationExtractors.OllieExtractor
import co.pemma.{FactorieFunctions, Utilities}

import scala.collection.GenSeq
import scala.io.Source

/**
 * Created by pv on 7/18/14.
 */
object DataManager
{

  def main(args: Array[String])
  {
//    val thing = "whippet"
            exportSentences(args(0).toLowerCase())
//    getRelations(readInSentences(s"data/$thing.data"), thing)

    //    val c = new ClauseIEExtractor
    //    println(c.extract(" the whippet is a sighthound breed that looks like a small greyhound .").mkString("\n"))
  }

  def exportSentences(thing : String)
  {
    val outputLocation = s"data/${thing}.data"

    // get documents from galago
    val galago = new ClueWebQuery
    val regexer = new Regexer("","")
    val queries = regexer.patternList.map(p => s"$thing ${p.replaceAll("\\?", "")}")


    val documents = galago.runBatchQueries(queries, 1000)


    println(documents.size)
    // convert documents to sentences
    var i = 0
    val sentences = documents.par.flatMap(document => {
      i += 1
      Utilities.printPercentProgress(i, documents.size)
      // doc -> sentences with factorie
      val strings = FactorieFunctions.extractSentences(load.LoadPlainText.fromString(document).head).map(_.string)
      // filter sentences that dont contain the thing
      strings.filter(_.contains(thing))
    })

    println(sentences.size)

    // export the sentences
    printSentences(sentences, outputLocation)
  }


  def printSentences(sentences : GenSeq[String], outputLocation : String)
  {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation)))
    sentences.foreach(sentence =>
    {
      println(s"\n ${sentence}")
      writer.println(s"\n ${sentence}")
    })
    writer.close()
  }

  def readInSentences(dataLocation :String) : Seq[String] =
  {
    val source = Source.fromFile(dataLocation, "iso-8859-1")
    source.getLines().filter(_.length > 10).filter(_!="\n").map(_.toLowerCase).toSeq
  }

  def getRelations(sentences : Seq[String], thing : String)// : Seq[Extraction] =
  {
    val extractor = new OllieExtractor
    val allExtractions = sentences.flatMap(extractor.extract(_))
    println(s" Found ${allExtractions.size} total relations")
    val filteredExtractions = allExtractions.filter(x =>{
      x.arg1.contains(thing) || x.arg2.contains(thing)
    })
    //    println(s" ${filteredExtractions.size} relations involve $thing")
    //    filteredExtractions.foreach(s => println(s.toString()))

    val filteredAgain = extractor.filter(filteredExtractions)
    println(s" ${filteredAgain.size} relations involve $thing and are a \'looks like\' relation")

    filteredAgain.foreach(s => println(s.sentence))
  }
}