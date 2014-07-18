package co.pemma.galagos

import cc.factorie.app.nlp.load
import cc.factorie.app.nlp.Sentence
import co.pemma.FactorieFunctions
import co.pemma.Utilities
import org.lemurproject.galago.core.parse.Document
import org.lemurproject.galago.core.retrieval.query.{Node, StructuredQuery}
import org.lemurproject.galago.core.retrieval.{Retrieval, ScoredDocument}
import org.lemurproject.galago.tupleflow.Parameters
import collection.JavaConversions._
import scala.collection.GenSeq


/**
 * Created by pat on 6/26/14.
 */
abstract class GalagoWrapper
{
  // how many results to return from search
  val DEFAULT_K = 2500
  val docComponents = new Document.DocumentComponents(true, false, false)
  val retrieval : Retrieval

  def runQuery(queryText : String) : GenSeq[String] =
  {
    runQuery(queryText, DEFAULT_K)
  }

  def runQuery(queryText : String, kResults : Int) : GenSeq[String] =
  {
    val results = getTopResults(queryText, kResults)
    // return the actual documents
    results.map(docId => retrieval.getDocument(docId.documentName, docComponents)).filterNot(_ == null).map(_.toString)
  }

  def runBatchQueries(queries : Seq[String]) : GenSeq[String] =
  {
    runBatchQueries(queries, DEFAULT_K)
  }

  def runBatchQueries(queries : Seq[String], kResults : Int) : GenSeq[String] =
  {
    val results = queries.flatMap(q => getTopResults(q, kResults)).toSet[ScoredDocument].toSeq
    // return the actual documents
    results.map(docId => retrieval.getDocument(docId.documentName, docComponents)).filterNot(_ == null).map(_.toString)
  }

  def retrieveMatchingSentences(queries : Seq[String], term : String, kResults : Int) : GenSeq[Sentence] =
  {
    val results = queries.flatMap(q => getTopResults(q, kResults))//.toSet[ScoredDocument].toSeq

    // return the sentences that contain the filter term
    println(s"Filtering sentences that match the contain the term \'$term\'")
    var i = 0
    results.par.flatMap(docId => {
      i += 1
      Utilities.printPercentProgress(i, results.size)
      val doc = retrieval.getDocument(docId.documentName, docComponents)
      if (doc != null) {
        FactorieFunctions.extractSentences(load.LoadPlainText.fromString(doc.toString).head).filter(_.contains(term))
      }
      else
        Seq()
    })
  }

  def getTopResults(queryText : String, kResults : Int) : Seq[ScoredDocument] =
  {
    val queryParams = mkQueryParams(queryText, kResults)

    // parse and transform query into runnable form
    val root: Node = StructuredQuery.parse(queryText)
    val transformed: Node = retrieval.transformQuery(root, queryParams)

    println(s"Querying galago for top $kResults results for '$queryText' ")
    // run query
    retrieval.executeQuery(transformed, queryParams).scoredDocuments.toList
  }

  def mkQueryParams(queryText :String, kResults : Int) : Parameters =
  {
    val query = new Parameters()
    query.set("text", s"#combine($queryText)")
    query.set("requested", kResults)
    query
  }
}
