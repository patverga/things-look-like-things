package co.pemma

import cc.factorie.app.nlp._
import edu.knowitall.ollie.confidence.OllieConfidenceFunction
import edu.knowitall.ollie.{Ollie, OllieExtraction}
import edu.knowitall.tool.parse.MaltParser
import edu.washington.cs.knowitall.extractor.ReVerbExtractor
import edu.washington.cs.knowitall.extractor.conf.ReVerbOpenNlpConfFunction
import edu.washington.cs.knowitall.nlp.OpenNlpSentenceChunker

/**
 * Created by pat on 6/30/14.
 */
object RelationExtractor
{
  // initialize MaltParser
  val parser =  new MaltParser
  val ollie = new Ollie
  val confidence = OllieConfidenceFunction.loadDefaultClassifier()

  def reverbExtraction(sentStr : String)
  {
    // Looks on the classpath for the default model files.
    val chunker = new OpenNlpSentenceChunker()
    val sent = chunker.chunkSentence(sentStr)

    // Prints out extractions from the sentence.
    val reverb = new ReVerbExtractor()
    val confFunc = new ReVerbOpenNlpConfFunction()

    val extractions = reverb.extract(sent).iterator()

    while(extractions.hasNext)
    {
      val extr = extractions.next()
      val conf = confFunc.getConf(extr)
      println(s"${extr.getArgument1}; ${extr.getRelation}; ${extr.getArgument2}")
      println("Conf=" + conf)
    }
  }

  def ollieExtraction(sentStr : String) : Iterable[(String, OllieExtraction, String)] =
  {
    try {
      val parsed = parser.dependencyGraph(sentStr)
      val extractionInstances = ollie.extract(parsed)
      val result = extractionInstances.map(inst => {
        val conf = confidence(inst)
        (("%.2f" format conf), inst.extraction, sentStr)
      })
      result
    }
    catch{
      case  e: Exception => System.err.println(s"MALT ERROR : $sentStr")
        Seq()
    }
  }

  /**
   *
   * @param documents
   * @return Iterable[(confidence, relation, full sentence)]
   */
  def extractRelations(documents : Seq[String]) : Iterable[(String, OllieExtraction, String)] =
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
          RelationExtractor.ollieExtraction(sentence.string.toLowerCase())
        }
        else
          Seq()
      })
      extractions
    })
    allExtractions
  }
}