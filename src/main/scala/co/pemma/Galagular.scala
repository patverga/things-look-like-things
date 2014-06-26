package co.pemma

import org.lemurproject.galago.core.tools.apps.BatchSearch
import org.lemurproject.galago.tupleflow.Parameters

/**
 * Created by pat on 6/26/14.
 */
object Galagular
{
  def main(args: Array[String])
  {
      queryGalago("test")
  }

  def queryGalago(query : String)
  {
    val args = Array[String](
      "--index=/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/",
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
      case e: Exception => {
        e.printStackTrace
      }
    }

    println(params)

    try {
      search.run(params, System.out)
    }
    catch {
      case e: Exception => {
        e.printStackTrace
      }
    }
  }
}
