package co.pemma.RelationExtractors

import edu.washington.cs.knowitall.extractor.ReVerbExtractor
import edu.washington.cs.knowitall.extractor.conf.ReVerbOpenNlpConfFunction
import edu.washington.cs.knowitall.nlp.OpenNlpSentenceChunker

/**
 * Created by pat on 7/11/14.
 */
class ReverbExtractor extends RelationExtractor{
  // initialize reverbs
  val chunker = new OpenNlpSentenceChunker()
  val reverb = new ReVerbExtractor()
  val confFunc = new ReVerbOpenNlpConfFunction()

  override def extract(sentStr : String): Iterable[Extraction] =
  {
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

}
