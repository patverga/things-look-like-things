package co.pemma.RelationExtractors

import edu.knowitall.ollie.Ollie
import edu.knowitall.ollie.confidence.OllieConfidenceFunction
import edu.knowitall.tool.parse.MaltParser

/**
 * Created by pat on 7/11/14.
 */
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
      case  e: Exception => System.err.println(s"MALT ERROR : $sentStr")
        Seq()
    }
  }

}
