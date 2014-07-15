package co.pemma.galagos

import org.lemurproject.galago.core.retrieval.RetrievalFactory
import org.lemurproject.galago.tupleflow.Parameters

/**
 * Created by pat on 7/15/14.
 */
class ClueWebQuery extends  GalagoWrapper
{
  val indexLocation = "/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/"
  println("cluein  " + indexLocation)

  val retrieval = RetrievalFactory.instance(indexLocation, new Parameters)
}
