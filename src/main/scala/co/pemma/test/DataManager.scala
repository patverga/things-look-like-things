package co.pemma.test

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import cc.factorie.app.nlp.load
import co.pemma.galagos.ClueWebQuery
import co.pemma.Regexer
import co.pemma.relationExtractors.OllieExtractor
import co.pemma.{FactorieFunctions, Utilities}
import edu.knowitall.ollie.{Ollie, OllieExtraction}
import edu.knowitall.ollie.confidence.OllieConfidenceFunction
import edu.knowitall.tool.parse.MaltParser

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
//    exportSentences2("whippet","whippet2.result")

//    exportRelationsByThing("whippet","whippet.result")
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
      val strings = FactorieFunctions.extractSentences(load.LoadPlainText.fromString(document).head).map(_.string.toLowerCase)
      // filter sentences that dont contain the thing
      strings.filter(_.contains(thing))
    })

    println(sentences.size)

    // export the sentences
    printSentences(sentences, outputLocation)
  }

  def exportSentences2(thing : String, outputLocation : String)
  {
    val galago = new ClueWebQuery

    val regexer = new Regexer(".*", ".*")
    val patternRegex = regexer.patternList.mkString("|")

    val queries = regexer.patternList.map(p => s"$thing ${p.replaceAll("\\?", "")}")
    val documents = galago.runBatchQueries(queries)
    var i = 0
    println("Processing Documents...")
    val filteredSentences = documents.flatMap(document =>
    {
      i += 1
      Utilities.printPercentProgress(i, documents.size)

      val doc = load.LoadPlainText.fromString(document).head
      val sentences = FactorieFunctions.extractSentences(doc)
      sentences.filter(sentence =>
      {
        val sentString = sentence.string
        val s = sentString.replaceAll("[^\\x00-\\x7F]", "").trim
        (s != "" && s != null && s.length > 10 && sentString.contains(thing))
      })
    })
    printSentences(filteredSentences, outputLocation)
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

  val omitArgRegex = "(?:you)|(?:he)|(?:she)|(?:it)|(?:we)|(?:they)|(?:him)|(?:her)|(?:i)|(?:\\W)"

  def exportRelationsByThing(thing : String, outputLocation : String)
  {
    val galago = new ClueWebQuery

    val regexer = new Regexer(".*", ".*")
    val patternRegex = regexer.patternList.mkString("|")
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation)))

    val queries = regexer.patternList.map(p => s"$thing ${p.replaceAll("\\?", "")}")
    val documents = galago.runBatchQueries(queries)

    val extractions = extractRelations(documents, thing)

    // filter relations that do not involve the 'thing'
    val filteredExtractions = extractions.filter(x => {
      (x._2.arg1.text.contains(thing) || x._2.arg2.text.contains(thing)) &&
        !x._2.arg1.text.matches(omitArgRegex) && !x._2.arg2.text.matches(omitArgRegex) &&
        x._2.rel.text.matches(patternRegex)
    })
    filteredExtractions.foreach(extract =>
    {
      println(s"${extract._1} ${extract._2}")
      writer.println(s"${extract._1} ${extract._2}")
    })
    writer.close()
  }
  // initialize MaltParser
  val parser =  new MaltParser
  val ollie = new Ollie
  val confidence = OllieConfidenceFunction.loadDefaultClassifier()

  def ollieExtraction(sentStr : String) : GenSeq[(String, OllieExtraction)] =
  {
    try {
      val parsed = parser.dependencyGraph(sentStr)
      val extractionInstances = ollie.extract(parsed)
      val result = extractionInstances.map(inst => {
        val conf = confidence(inst)
        (("%.2f" format conf), inst.extraction)
      })
      result.toSeq
    }
    catch{
      case e: Exception => System.err.println(s"MALT ERROR : $sentStr")
        Seq()
    }
  }

  def extractRelations(documents : GenSeq[String], thing :String) : GenSeq[(String, OllieExtraction)] =
  {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter("whippet3.result")))

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
        val sentString = sentence.string
        val s = sentString.replaceAll("[^\\x00-\\x7F]", "").trim
        if (s != "" && s != null && s.length > 10)
        {
          if (sentString.contains(thing))
          writer.println(s"${sentString.toLowerCase()}\n")
          ollieExtraction(sentString.toLowerCase())
        }
        else
          Seq()
      })
      extractions
    })
    writer.close()
    allExtractions
  }
}