package co.pemma.RelationExtractors

import de.mpii.clausie.{Proposition, ClausIE}

/**
 * Created by pat on 7/14/14.
 */
class ClauseIEExtractor  () extends RelationExtractor {

  import collection.JavaConversions._

  val clausIE = new ClausIE
  clausIE.initParser

  override def extract(sentence: String): Iterable[Extraction] = {
    try {
      clausIE.parse(sentence)
      clausIE.detectClauses
      clausIE.generatePropositions

      val results = clausIE.getPropositions.toSet[Proposition].map(prop =>
      {
        println(s"${prop.subject()}  ${prop.relation()}  ${prop.argument(0)}")
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

