package co.pemma

import cc.factorie.app.nlp.load.ChunkTag
import cc.factorie.app.nlp._
import cc.factorie.app.nlp.phrase.{NounPhraseList, PosBasedNounPhraseFinder, NPChunkMentionFinder}

/**
 * Created by pat on 6/30/14.
 */
object NounPhraseChunker {

  def main(args: Array[String]) {
    val pipeline = new DocumentAnnotationPipeline(Seq(
      segment.DeterministicTokenizer,
      segment.DeterministicSentenceSegmenter,
      pos.OntonotesForwardPosTagger,
      PosBasedNounPhraseFinder
    ))

    val doc = load.LoadPlainText.fromString("The person who dances is fast. The man chopping wood is dumb." +
      "There was a big red tree over there.").head

    pipeline.process(doc)

    doc.attr[NounPhraseList].foreach(tok => {
      println(tok)
//      val chunk = tok.attr[ChunkTag]
//      if (chunk != null)
//        println(chunk.categoryValue)
    })

  }
}