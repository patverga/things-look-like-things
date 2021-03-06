package co.pemma

import org.lemurproject.galago.core.parse.Document
import org.lemurproject.galago.core.retrieval.query.{Node, StructuredQuery}
import org.lemurproject.galago.core.retrieval.{Retrieval, RetrievalFactory, ScoredDocument}
import org.lemurproject.galago.tupleflow.Parameters

import scala.collection.GenSeq
import scala.collection.JavaConversions._


/**
 * Created by pat on 6/26/14.
 */
abstract class GalagoWrapper
{
  // how many results to return from search
  val DEFAULT_K = 1000
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
    results.map(docId => retrieval.getDocument(docId.documentName, docComponents)).filter(_ != null).map(_.toString)
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

class ClueWebQuery extends  GalagoWrapper
{
  val indexLocation = "/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/"
  println("cluein  " + indexLocation)

  val retrieval = RetrievalFactory.instance(indexLocation, new Parameters)
}

class WikipediaQuery extends  GalagoWrapper
{
  val indexLocation = "/mnt/nfs/work1/pat/wikipedia/index"
  println("wikin  " + indexLocation)
  val retrieval = RetrievalFactory.instance(indexLocation, new Parameters)
}

class WikiClueQuery extends  GalagoWrapper
{
  val clueWebIndex = "/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/"
  val wikiIndex = "/mnt/nfs/work1/pat/wikipedia/index"
  val indices = seqAsJavaList(Seq(clueWebIndex, wikiIndex))
  println("Using ClueWeb12 + Wikipedia Index")
  val retrieval = RetrievalFactory.instance(indices, new Parameters)
}

