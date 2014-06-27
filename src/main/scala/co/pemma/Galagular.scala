package co.pemma

import java.io.{PrintStream, ByteArrayOutputStream}

import org.lemurproject.galago.core.parse.Document
import org.lemurproject.galago.core.retrieval.query.{StructuredQuery, Node}
import org.lemurproject.galago.core.retrieval.{Retrieval}
import org.lemurproject.galago.core.tools.apps.{DumpDocFn, BatchSearch}
import org.lemurproject.galago.tupleflow.Parameters

/**
 * Created by pat on 6/26/14.
 */
object Galagular extends GalagoWrapper("/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/", true, false, false)
{
  val INDEX_LOCATION = "/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/"

  def main(args: Array[String])
  {
//    val docList = getDocumentsForQueryTerms("test")
//    println(docList.size)
    runQuery("test query")
  }

  def getDocumentsForQueryTerms(query : String) : collection.mutable.MutableList[String] =
  {
    //  MAKE THIS WORK, HAVE IT USE INTERNAL FUNCTIONS
    val resultIds = queryGalago(query)

    // retrieve each document and put it in a list
    val docList = collection.mutable.MutableList[String]()
    resultIds.foreach(docId =>
    {
      val document: Document = retrieval.getDocument(docId, docComponents)
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

  /**
   * Takes a query string and returns the top 1000 most relevant docs
   * @param query The query terms to search for
   * @return ids of the top 1000 documents for the search query
   */
  def queryGalago(query : String) : collection.mutable.MutableList[String] =
  {
    val args = Array[String](
      "--index=" + INDEX_LOCATION,
      "--query=#combine("+query+")"
    )
    val params = argsToParams(args)
    val baos = new ByteArrayOutputStream()
    val stream = new PrintStream(baos)

    val search: BatchSearch = new BatchSearch()
    try
    {
      println("Performing search for query : \"" + query + "\"")
      search.run(params, stream)
    }
    catch {
      case e: Exception => {
        e.printStackTrace
      }
    }

    val resultString = baos.toString("UTF8")
    val results = resultString.split("\n")

    val docList = collection.mutable.MutableList[String]()
    results.foreach ( r =>
    {
      val doc = r.split("\\s+")(2)
      docList += doc
    })
    stream.close()

    docList
  }

  def runQuery(queryText : String)
  {
    val args = Array[String](
      "--query=#combine("+queryText+")"
    )
    val params = argsToParams(args)
    // get queries
    val queries: java.util.List[Parameters] = BatchSearch.collectQueries(params)
    val query = queries.get(0)

    // parse and transform query into runnable form
    val root: Node = StructuredQuery.parse(queryText)
    val transformed: Node = retrieval.transformQuery(root, query)

    // run query
    val results = retrieval.executeQuery(transformed, query).scoredDocuments

    // print results
    var i = 0
    while (i < results.size())
    {
      println(results.get(i).toString())
      i += 1
    }
  }


  def argsToParams(args : Array[String]) : Parameters =
  {
    var params: Parameters = null
    try
    {
      params = Parameters.parseArgs(args)
    }
    catch {
      case e: Exception =>
      {
        e.printStackTrace
      }
    }
    params
  }

  def getSingleDocument(doc : String) : String =
  {
    val args = Array[String](
      "--index=" + INDEX_LOCATION,
      "--id=" + doc,
      "--text=true",
      "--tokenize=false",
      "--metadata=false"
    )
    val params = argsToParams(args)
    val baos = new ByteArrayOutputStream()
    val stream = new PrintStream(baos)

    val docDumper = new DumpDocFn()
    docDumper.run(params, stream)

    val resultString = baos.toString("UTF8")
    resultString
  }


}
