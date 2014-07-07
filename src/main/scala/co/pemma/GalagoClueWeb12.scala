package co.pemma

import scala.collection.JavaConverters._
import org.lemurproject.galago.core.retrieval.query.{StructuredQuery, Node}
import org.lemurproject.galago.core.retrieval.ScoredDocument
import org.lemurproject.galago.tupleflow.Parameters

/**
 * Created by pat on 6/26/14.
 */
object GalagoClueWeb12 extends GalagoWrapper("/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/", true, false, false)
{
  // how many results to return from search
  val K_RESULTS = 1000

  def getDocumentsForQueryTerms(query : String) :  Seq[String] =
  {
    // retrieve each document and put it in a list
      runQuery(query).map(docId => retrieval.getDocument(docId.documentName, docComponents)).filterNot(_ == null).map(_.toString)
  }

  def runQuery(queryText : String) : collection.mutable.Buffer[ScoredDocument] =
  {

    val query = new Parameters()
    query.set("text", s"#combine($queryText)")
    query.set("requested", K_RESULTS)

    // parse and transform query into runnable form
    val root: Node = StructuredQuery.parse(queryText)
    val transformed: Node = retrieval.transformQuery(root, query)

    println(s"Querying galago for top $K_RESULTS results for '$queryText' ")
    // run query
    val results = retrieval.executeQuery(transformed, query).scoredDocuments
    results.asScala
  }

  def main(args: Array[String])
  {
    val docList = getDocumentsForQueryTerms("test")
    println(docList.size)
  }
}
