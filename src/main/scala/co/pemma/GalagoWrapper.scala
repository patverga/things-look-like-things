package co.pemma

import org.lemurproject.galago.core.parse.Document
import org.lemurproject.galago.core.retrieval.query.{Node, StructuredQuery}
import org.lemurproject.galago.core.retrieval.{RetrievalFactory, ScoredDocument}
import org.lemurproject.galago.tupleflow.Parameters
import scala.collection.JavaConverters._

/**
 * Created by pat on 6/26/14.
 */
object GalagoWrapper
{
  // how many results to return from search
  val DEFAULT_K = 1000
  val indexLocation = "/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/"
  val retrieval = RetrievalFactory.instance(indexLocation, new Parameters)
  val docComponents = new Document.DocumentComponents(true, false, false)

  def runQuery(queryText : String) : Seq[String] =
  {
    runQuery(queryText, DEFAULT_K)
  }

  def runQuery(queryText : String, kResults : Int) : Seq[String] =
  {
    val results = getTopResults(queryText, kResults)
    // return the actual documents
    results.map(docId => retrieval.getDocument(docId.documentName, docComponents)).filterNot(_ == null).map(_.toString)
  }

  def runBatchQueries(queries : Seq[String]) : Seq[String] =
  {
    val results = queries.flatMap(q => getTopResults(q, DEFAULT_K)).toSet[ScoredDocument].toSeq
    // return the actual documents
    results.map(docId => retrieval.getDocument(docId.documentName, docComponents)).filterNot(_ == null).map(_.toString)
  }

  def getTopResults(queryText : String, kResults : Int) : Seq[ScoredDocument] =
  {
    val queryParams = mkQueryParams(queryText, kResults)

    // parse and transform query into runnable form
    val root: Node = StructuredQuery.parse(queryText)
    val transformed: Node = retrieval.transformQuery(root, queryParams)

    println(s"Querying galago for top $kResults results for '$queryText' ")
    // run query
    retrieval.executeQuery(transformed, queryParams).scoredDocuments.asScala
  }

  def mkQueryParams(queryText :String, kResults : Int) : Parameters =
  {
    val query = new Parameters()
    query.set("text", s"#combine($queryText)")
    query.set("requested", kResults)
    query
  }
}
