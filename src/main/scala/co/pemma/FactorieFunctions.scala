package co.pemma

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.load.ChunkTag
import cc.factorie.app.nlp.phrase.BILOUChainChunker

/**
 * Created by pat on 7/10/14.
 */
object FactorieFunctions {
  val pipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter))


  //  def main(args: Array[String])
  //  {
  //    //    val sentences = Array(
  //    //      "Whippets look like small greyhouds.",
  //    //      "I think it looked like a dog.",
  //    //      "People are tall.",
  //    //      "This sentence is a test."
  //    //    )
  //    //    sentences.foreach(s => {
  //    //      println(s)
  //    //      NLPThings.ollieExtraction(s)
  //    //      NLPThings.reverbExtraction(s)
  //    //    })
  //    chunkNounPhrase(" hehe , looks like charlie manson to me .  lcd plasma television appear like works of art hanging in your living room ")
  //  }
  def main(args: Array[String]) {
    val clause = new ClauseIEExtractor
    val sent = " lcd plasma television appear like works of art hanging in your living room "
    clause.extract(sent)
  }


  def extractSentences(docString : String) : Iterable[Sentence] =
  {
    val doc = load.LoadPlainText.fromString(docString).head
    extractSentences(doc, pipeline)
  }

  def extractSentences(doc :Document) : Iterable[Sentence] =
  {
    extractSentences(doc, pipeline)
  }

  def extractSentences(doc : Document, pipe : DocumentAnnotationPipeline ) : Iterable[Sentence] =
  {
    try {
      val processedDoc = pipe.process(doc)
      processedDoc.sentences
    }
    catch
      {
        case  e: Exception => System.err.println(s"FACTORIE ERROR : ${e.printStackTrace()}")
          Iterable()
        case  e: StackOverflowError => System.err.println(s"FACTORIE ERROR : ${doc.string}")
          Iterable()
      }
  }


  def chunkNounPhrase(sentence : String)
  {
    val chunkPipeline = new DocumentAnnotationPipeline(
      Seq(segment.DeterministicTokenizer,
        segment.DeterministicSentenceSegmenter,
        pos.OntonotesForwardPosTagger,
        BILOUChainChunker
      ))

    //    val source = io.Source.fromFile("results/pattern/lookslike.result")
    //    val fileDoc = load.LoadPlainText.fromSource(source)
    //    val docString = fileDoc.head.string.replaceAll("\\)\n",". ").replaceAll("[0-9]\\.[0-9][0-9] \\(","")
    val doc = load.LoadPlainText.fromString(sentence).head
    chunkPipeline.process(doc)

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
