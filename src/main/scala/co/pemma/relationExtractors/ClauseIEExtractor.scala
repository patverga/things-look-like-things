package co.pemma.relationExtractors

import de.mpii.clausie.{Proposition, ClausIE}
import collection.JavaConversions._

/**
 * Created by pat on 7/14/14.
 */
class ClauseIEExtractor  () extends RelationExtractor
{
  val clausIE = new ClausIE
  clausIE.initParser

  override def extract(sentence: String): Iterable[Extraction] = {
    try {
      clausIE.parse(sentence)
      clausIE.detectClauses
      clausIE.generatePropositions

      val results = clausIE.getPropositions.toSet[Proposition].map(prop =>
      {
        new Extraction(1., prop.subject, prop.relation, prop.argument(0), sentence)
      })
      results
    }
    catch {
      case e: Exception => System.err.println(s"CLAUSE IE ERRORED SOMEWHERE : $sentence")
        Seq()
      case  e: StackOverflowError => System.err.println(s"STANFORD ERROR : $sentence")
        Seq()
    }
  }
}

