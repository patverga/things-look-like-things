package co.pemma

import java.io.{PrintStream, ByteArrayOutputStream, InputStream}

import org.lemurproject.galago.core.tools.apps.BatchSearch
import org.lemurproject.galago.tupleflow.Parameters

/**
 * Created by pat on 6/26/14.
 */
object Galagular
{

  val INDEX_LOCATION = "/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/"

  def main(args: Array[String])
  {
      queryGalago("test")
  }

  def queryGalago(query : String)
  {

    val args = Array[String](
      "--index=" + INDEX_LOCATION,
      "--query=#combine("+query+")"
    )

    //        args = new String[]
    //                {
    //                        "--index=/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/",
    //                        "--id=$file",
    //                        "--text=true",
    //                        "--tokenize=false",
    //                        "--metadata=false"
    //                };

    val search: BatchSearch = new BatchSearch()

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

    println(params)

    val baos = new ByteArrayOutputStream()
    val stream = new PrintStream(baos)

    try {
      search.run(params, stream)
    }
    catch {
      case e: Exception => {
        e.printStackTrace
      }
    }

    val results = baos.toString("UTF8")

    println(results)

    stream.close()
  }
}
