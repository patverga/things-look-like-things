package co.pemma

import java.io.{PrintStream, ByteArrayOutputStream}

import org.lemurproject.galago.core.tools.apps.{DumpDocFn, BatchSearch}
import org.lemurproject.galago.tupleflow.Parameters

/**
 * Created by pat on 6/26/14.
 */
object Galagular
{

  val INDEX_LOCATION = "/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/"

  def main(args: Array[String])
  {
    val results = queryGalago("test")

    results.foreach(r => println(r))
  }

  def getDocument(doc : String)
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
    val results = resultString.split("\n")

    println(results.toString)
  }

  /**
   * Takes a query string and returns the top 1000 most relevant docs
   * @param query The query terms to search for
   * @return the top 1000 documents for the search query
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


}
