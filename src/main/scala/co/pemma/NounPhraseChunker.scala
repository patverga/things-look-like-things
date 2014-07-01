package co.pemma

import cc.factorie.app.nlp.load.ChunkTag
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.phrase.BILOUChainChunker

/**
 * Created by pat on 6/30/14.
 */
object NounPhraseChunker {

  def main(args: Array[String]) {
    val pipeline = new DocumentAnnotationPipeline(Seq(
      segment.DeterministicTokenizer,
      segment.DeterministicSentenceSegmenter,
      pos.OntonotesForwardPosTagger,
      BILOUChainChunker
    ))

    val source = io.Source.fromFile("/home/pat/things-look-like-things/target/classes/things","ISO-8859-1")
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
}