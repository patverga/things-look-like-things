package co.pemma

import scala.collection.JavaConverters._
import org.lemurproject.galago.core.parse.Document
import org.lemurproject.galago.core.retrieval.query.{StructuredQuery, Node}
import org.lemurproject.galago.core.retrieval.ScoredDocument
import org.lemurproject.galago.tupleflow.Parameters

/**
 * Created by pat on 6/26/14.
 */
object GalagoClueWeb12 extends GalagoWrapper("/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/", true, false, false)
{
  // how many results to return from search
  val K_RESULTS = 10

  def getDocumentsForQueryTerms(query : String) : collection.mutable.MutableList[String] =
  {
    val resultIds = runQuery(query)

    // retrieve each document and put it in a list
    val docList = collection.mutable.MutableList[String]()
    resultIds.foreach(docId =>
    {
      val document: Document = retrieval.getDocument(docId.documentName, docComponents)
      if (document != null)
      {
        docList += document.toString
      }
      else {
        println("Document " + docId + " does not exist in index.")
      }
    })
    docList
  }

  def runQuery(queryText : String) : collection.mutable.Buffer[ScoredDocument] =
  {

    val query = new Parameters()
    query.set("text", s"#combine($queryText)")
    query.set("requested", s"$K_RESULTS")

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
