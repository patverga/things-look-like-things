package co.pemma

import cc.factorie.app.nlp._
import co.pemma.Util.Utilities
import de.mpii.clausie.{ClausIE, Proposition}
import edu.knowitall.ollie.Ollie
import edu.knowitall.ollie.confidence.OllieConfidenceFunction
import edu.knowitall.tool.parse.MaltParser
import edu.washington.cs.knowitall.extractor.ReVerbExtractor
import edu.washington.cs.knowitall.extractor.conf.ReVerbOpenNlpConfFunction
import edu.washington.cs.knowitall.nlp.OpenNlpSentenceChunker

import scala.collection.GenSeq
import scala.collection.JavaConversions._

/**
 * Created by pat on 6/30/14.
 */
abstract class RelationExtractor
{
  val omitArgRegex = "(?:you)|(?:he)|(?:she)|(?:it)|(?:we)|(?:they)|(?:him)|(?:her)|(?:i)|(?:\\W)|(?:one)".r
  val patternRegex = "(?:(?:appear(?:s|ed|ance is)?|look(?:s|ed)?) (?:exactly |almost |pretty much |just )?(?:the same as|identical to|similar to|like)|(?:resemble(?:s|d)))".r


  def extractRelations(documents : GenSeq[String]) : GenSeq[Extraction] = {
    return extractRelations(documents, "")
  }

  def extractRelations(documents : GenSeq[String], thingRegex : String) : GenSeq[Extraction] =
  {
    var i = 0
    val sentences = documents.flatMap(document => {
      i += 1
      Utilities.printPercentProgress(i, documents.size)

      val doc = load.LoadPlainText.fromString(document).head
      // doc -> sentences with factorie, keep only sentences that match our pattern

      FactorieFunctions.extractSentences(doc).filter(x => {
        x != null && x.length > 10 && x.contains(thingRegex)
      })
    })

    filter(sentences.flatMap(extract(_)))
  }

  def extract(sentence : Sentence) : Iterable[Extraction] =
  {
    extract (sentence.string)
  }

  def extract(sentStr : String) : Iterable[Extraction]

  def filter(extractions : GenSeq[Extraction]) : GenSeq[Extraction] =
  {
    // filter out relations that we dont want
    extractions.filter(x => {
     patternRegex.pattern.matcher(x.rel).matches() &&
        !omitArgRegex.pattern.matcher(x.arg1).matches &&
        !omitArgRegex.pattern.matcher(x.arg2).matches
    })
  }
}

////////////////// OLLIE //////////////////
class OllieExtractor () extends RelationExtractor
{
  // initialize ollies
  val parser =  new MaltParser
  val ollie = new Ollie
  val confidence = OllieConfidenceFunction.loadDefaultClassifier()

  override def extract(sentStr : String) : Iterable[Extraction] =
  {
    try {
      val parsed = parser.dependencyGraph(sentStr)
      val extractionInstances = ollie.extract(parsed)
      val result = extractionInstances.map(inst => {
        val conf = confidence(inst)
        new Extraction(conf, inst.extraction.arg1.text, inst.extraction.rel.text, inst.extraction.arg2.text, sentStr)
      })
      result
    }
    catch{
      case  e: Exception =>
        System.err.println(s"MALT ERROR : $sentStr")
        System.err.println(e.printStackTrace())
        Seq()
    }
  }

}

////////////////// REVERB ////////////////////
class ReverbExtractor extends RelationExtractor{
  // initialize reverbs
  val chunker = new OpenNlpSentenceChunker()
  val reverb = new ReVerbExtractor()
  val confFunc = new ReVerbOpenNlpConfFunction()

  override def extract(sentStr : String): Iterable[Extraction] =
  {
    try{
      // Looks on the classpath for the default model files.
      val sent = chunker.chunkSentence(sentStr)

      // Prints out extractions from the sentence.
      val extractions = reverb.extract(sent).iterator()

      val result = scala.collection.mutable.MutableList[Extraction]()
      while(extractions.hasNext)
      {
        val extr = extractions.next()
        val conf = confFunc.getConf(extr)
        result += new Extraction(conf, extr.getArgument1.getText, extr.getRelation.getText, extr.getArgument2.getText, sentStr)
      }
      result
    }
    catch{
      case  e: Exception =>
        System.err.println(s"REVERB ERROR : $sentStr")
        System.err.println(e.printStackTrace())
        Seq()
    }
  }
}

//////////////////// CLAUSIE ///////////////////
class ClauseIEExtractor  () extends RelationExtractor
{
  val relationRegex = ".*(?:appear(?:ed|ance is)?|look(?:s|ed)?) (?:exactly |almost| pretty much)?(?:the same as|identical to|similar to|like).*".r

  val clausIE = new ClausIE
  clausIE.initParser

  override def extract(sentence: String): Iterable[Extraction] = {

    generatePropositions(sentence)

    try {
      val results = clausIE.getPropositions.toSet[Proposition].filter(_.noArguments() > 0).map(prop =>
      {
        new Extraction(1., prop.subject, prop.relation, prop.argument(0), sentence)
      })
      results
    }
    catch {
      case e: Exception =>
        System.err.println(s"CLAUSE IE ERRORED SOMEWHERE : $sentence")
        System.err.println(e.printStackTrace)
        Seq()
      case  e: StackOverflowError =>
        System.err.println(s"STANFORD ERROR : $sentence")
        Seq()
    }
  }

  def generatePropositions(sentence : String)
  {
    try {
      clausIE.parse(sentence)
      clausIE.detectClauses
      clausIE.generatePropositions
    }
    catch {
      case e: Exception =>
        System.err.println(s"CLAUSE IE ERRORED SOMEWHERE : $sentence")
        System.err.println(e.printStackTrace)
        Seq()
      case e: StackOverflowError =>
        System.err.println(s"STANFORD ERROR : $sentence")
        Seq()
    }
  }

  def getClauses(sentence : String) : String =
  {
    generatePropositions(sentence)
    val clauses = clausIE.getClauses
    val clauses2 = clauses.map(clause => clause.toString(clausIE.getOptions()))
    clauses2.mkString("           ")

  }

  override def filter(extractions : GenSeq[Extraction]) : GenSeq[Extraction] =
  {
    // filter out relations that we dont want
    extractions.filter(x => {
      //      relationRegex.pattern.matcher(x.toString).matches &&
      !omitArgRegex.pattern.matcher(x.arg1).matches &&
        !omitArgRegex.pattern.matcher(x.arg2).matches
    })
  }
}


/////////////////// EXTRACTION OBJECT ////////////////
class Extraction(c : Double, a1 : String, r : String, a2 : String, s : String)
{
  val arg1 = a1.trim
  val rel = r.trim
  val arg2 = a2.trim
  val sentence = s
  val confidence = ("%.2f" format c).toDouble

  def relation() : String =
  {
    s"$confidence ($arg1; $rel; $arg2)"
  }

  override def toString() : String =
  {
    s"$arg1 $rel $arg2"
  }
}