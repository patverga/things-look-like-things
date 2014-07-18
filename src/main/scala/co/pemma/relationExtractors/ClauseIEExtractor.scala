package co.pemma.relationExtractors

import de.mpii.clausie.{Proposition, ClausIE}
import collection.JavaConversions._
import scala.collection.GenSeq

/**
 * Created by pat on 7/14/14.
 */
class ClauseIEExtractor  () extends RelationExtractor
{
  val relationRegex = ".*(?:appear(?:ed|ance is)?|look(?:s|ed)?) (?:exactly |almost| pretty much)?(?:the same as|identical to|similar to|like).*".r

  val clausIE = new ClausIE
  clausIE.initParser

  override def extract(sentence: String): Iterable[Extraction] = {

    generatePropositions(sentence)

    try {
      val results = clausIE.getPropositions.toSet[Proposition].filter(_.noArguments() > 0).map(prop =>
      {
        new Extraction(1., prop.subject, prop.relation, prop.argument(0), sentence)
      })
      results
    }
    catch {
      case e: Exception =>
        System.err.println(s"CLAUSE IE ERRORED SOMEWHERE : $sentence")
        System.err.println(e.printStackTrace)
        Seq()
      case  e: StackOverflowError =>
        System.err.println(s"STANFORD ERROR : $sentence")
        Seq()
    }
  }

  def generatePropositions(sentence : String)
  {
    try {
      clausIE.parse(sentence)
      clausIE.detectClauses
      clausIE.generatePropositions
    }
    catch {
      case e: Exception =>
        System.err.println(s"CLAUSE IE ERRORED SOMEWHERE : $sentence")
        System.err.println(e.printStackTrace)
        Seq()
      case e: StackOverflowError =>
        System.err.println(s"STANFORD ERROR : $sentence")
        Seq()
    }
  }

  def getClauses(sentence : String) : String =
  {
    generatePropositions(sentence)
    val clauses = clausIE.getClauses.toList
    val clauses2 = clauses.map(clause => clause.toString(clausIE.getOptions()))
    clauses2.mkString("           ")

  }

  override def filter(extractions : GenSeq[Extraction]) : GenSeq[Extraction] =
  {
    // filter out relations that we dont want
    extractions.filter(x => {
//      relationRegex.pattern.matcher(x.toString).matches &&
        !omitArgRegex.pattern.matcher(x.arg1).matches &&
        !omitArgRegex.pattern.matcher(x.arg2).matches
    })
  }
}

