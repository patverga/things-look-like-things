package co.pemma

import cc.factorie.app.nlp.load.ChunkTag
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.phrase.BILOUChainChunker
import edu.washington.cs.knowitall.extractor.ReVerbExtractor
import edu.washington.cs.knowitall.extractor.conf.{ReVerbOpenNlpConfFunction, ConfidenceFunction}
import edu.washington.cs.knowitall.nlp.{ChunkedSentence, OpenNlpSentenceChunker}

/**
 * Created by pat on 6/30/14.
 */
object NLPThings {

  def main(args: Array[String]) 
  {
    NLPThings.relationExtractor("your glasses are awesome and my mommy and i can t wait to see what you look like in them.")

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

  def relationExtractor(sentStr : String)
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
      println("Arg1=" + extr.getArgument1())
      println("Rel=" + extr.getRelation())
      println("Arg2=" + extr.getArgument2())
      println("Conf=" + conf)
    }
  }
}