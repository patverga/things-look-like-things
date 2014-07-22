package co.pemma.test

import java.io.{FileWriter, BufferedWriter, PrintWriter}

import cc.factorie.app.nlp.load
import co.pemma.Util.{ProgressBar, Utilities}
import co.pemma.{ClueWebQuery, Extraction, FactorieFunctions, OllieExtractor, Regexer}

import scala.collection.{GenSeq, GenSet}
import scala.io.Source

/**
 * Created by pv on 7/18/14.
 */
object DataManager
{

  val newPatternLocation = "new_patterns"

  def main(args: Array[String])
  {
//    val thing = "whippet"
    //            exportSentences(args(0).toLowerCase())
    //    exportSentences2("whippet","whippet2.result")

    //    exportRelationsByThing("whippet","whippet.result")
//    getRelations(readInSentences(s"data/$thing.data"), thing)
    relationsWithThing(readInSentences(s"data/$thing.data"), thing)
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
      val strings = FactorieFunctions.extractSentences(load.LoadPlainText.fromString(document).head).map(_.string.toLowerCase)
      // filter sentences that dont contain the thing
      strings.filter(_.contains(thing))
    })

    println(sentences.size)

    // export the sentences
    printSentences(sentences, outputLocation)
  }

  def readInSentences(dataLocation :String) : Seq[String] =
  {
    println(s"Reading in data from $dataLocation")
    val source = Source.fromFile(dataLocation, "iso-8859-1")
    source.getLines().filter(_.length > 10).filter(_!="\n").map(_.toLowerCase).toSeq
  }

  def getRelations(sentences : Seq[String], thing : String)
  {
    println(s"Extracting Relations for $thing")
    val extractor = new OllieExtractor
    val progress = new ProgressBar(sentences.size)
    val allExtractions = sentences.flatMap(s => {
      progress.increment()
      extractor.extract(s)
    })
    println(s" Found ${allExtractions.size} total relations")
    val filteredExtractions = allExtractions.filter(x =>{
      (x.arg1.contains(thing) || x.arg2.contains(thing))
    })

    //    println(s" ${filteredExtractions.size} relations involve $thing")
    //    filteredExtractions.foreach(s => println(s.toString()))

        val filteredAgain = extractor.filter(filteredExtractions)
    println(s" ${filteredAgain.size} relations involve $thing and are a \'looks like\' relation")

    filteredAgain.foreach(s => println(s.relation()))
  }

  def relationsWithThing(sentences : Seq[String], thing : String)
  {
    println(s"Extracting Relations for $thing")
    val extractor = new OllieExtractor
    val progress = new ProgressBar(sentences.size)
    val allExtractions = sentences.flatMap(s => {
      progress.increment()
      extractor.extract(s)
    })
    val filteredExtractions = allExtractions.filter(x =>{
      (x.arg1.contains(thing) || x.arg2.contains(thing)) //&& x.confidence >= 0.80
    })
    println(s" Found ${allExtractions.size} total relations. Found ${filteredExtractions.size} relations involving $thing.")

    argsFromRelations(filteredExtractions.toSet[Extraction], extractor.filter(filteredExtractions).toSet[Extraction], thing)
  }

  def argsFromRelations( thingRelations : GenSet[Extraction], patternRelations : GenSet[Extraction], thing : String)
  {
    patternRelations.foreach(p => println(p.relation()))
    println("getting other args / patterns now")
    // pattern relations are either 'x looks like thing' or 'thing looks like x', extract x
    val otherArgs = patternRelations.map(rel =>
    {
      if (rel.arg1.contains(thing))
        rel.arg2
      else
        rel.arg1
    }).filterNot(_.contains(thing)) // dont want ' thing looks like thing'

    val otherPatternRelations = thingRelations.diff(patternRelations)

    val otherPatterns = otherPatternRelations.filter(x =>
    {
     otherArgs.contains(x.arg1) || otherArgs.contains(x.arg2)
    }).map(_.rel)

    printSentences(otherPatterns, s"$newPatternLocation/$thing")
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
}