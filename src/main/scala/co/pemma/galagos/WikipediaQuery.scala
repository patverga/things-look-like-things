package co.pemma.galagos

import org.lemurproject.galago.core.retrieval.RetrievalFactory
import org.lemurproject.galago.tupleflow.Parameters

/**
 * Created by pat on 7/15/14.
 */
class WikipediaQuery extends  GalagoWrapper
{
  val indexLocation = "/mnt/nfs/work1/pat/wikipedia/index"
  println("wikin  " + indexLocation)
  val retrieval = RetrievalFactory.instance(indexLocation, new Parameters)
}

