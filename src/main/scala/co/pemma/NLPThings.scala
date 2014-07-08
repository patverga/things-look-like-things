package co.pemma

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.load.ChunkTag
import cc.factorie.app.nlp.phrase.BILOUChainChunker
import edu.knowitall.ollie.{OllieExtraction, Ollie}
import edu.knowitall.ollie.confidence.OllieConfidenceFunction
import edu.knowitall.tool.parse.MaltParser
import edu.washington.cs.knowitall.extractor.ReVerbExtractor
import edu.washington.cs.knowitall.extractor.conf.ReVerbOpenNlpConfFunction
import edu.washington.cs.knowitall.nlp.OpenNlpSentenceChunker

/**
 * Created by pat on 6/30/14.
 */
object NLPThings
{
  // initialize MaltParser
  val parser =  new MaltParser
  val ollie = new Ollie
  val confidence = OllieConfidenceFunction.loadDefaultClassifier()

  def main(args: Array[String])
  {
  val sentences = Array(
  "Whippets look like small greyhouds.",
  "I think it looked like a dog.",
  "People are tall.",
  "This sentence is a test."
  )
    sentences.foreach(s => {
      println(s)
      NLPThings.ollieExtraction(s)
      NLPThings.reverbExtraction(s)
    })
  }

  def chunkNounPhrase()
  {
    val pipeline = new DocumentAnnotationPipeline(
      Seq(segment.DeterministicTokenizer,
        segment.DeterministicSentenceSegmenter,
        pos.OntonotesForwardPosTagger,
        BILOUChainChunker
      ))

    val source = io.Source.fromFile("/home/pat/things-look-like-things/target/classes/things")
    val fileDoc = load.LoadPlainText.fromSource(source)
    val docString = fileDoc.head.string.replaceAll("\n",". ")
    val doc = load.LoadPlainText.fromString(docString).head

    //    val doc = load.LoadPlainText.fromString(
    //      "A cartoon is a form of two-dimensional illustrated visual art." +
    //        "lifting machine parts." +
    //        "making winning gestures." +
    //        "pulling pushing a vehicle." +
    //        "putting ring on finger." +
    //        "scrubbing appliance by hand." +
    //        "standing on top of bike." +
    //        "wiping down an appliance." +
    //        "working on a table top machine." +
    //        "animal chewing an object." +
    //        "animals chasing\n" +
    //        "bride and groom standing in front of a priest official."
    //    ).head

    pipeline.process(doc)

    doc.tokens.foreach(tok => {
      print(tok.string)
      val chunk = tok.attr[ChunkTag]
      if (chunk != null)
        print(s"(${chunk.categoryValue})")
      if (tok.string.equals("."))
        println()
    })
  }

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

  def ollieExtraction(sentStr : String) : Iterable[(String, OllieExtraction)] =
  {
    try {
      val parsed = parser.dependencyGraph(sentStr)
      val extractionInstances = ollie.extract(parsed)
      println("Extractions:")
      val result = extractionInstances.map(inst => {
        val conf = confidence(inst)
        (("%.2f" format conf), inst.extraction)
      })
      result
    }
    catch{
      case  e: Exception => println(s"MALT ERROR : $sentStr")
        null
    }
  }
}