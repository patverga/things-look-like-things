package co.pemma.galagos

import org.lemurproject.galago.core.retrieval.RetrievalFactory
import org.lemurproject.galago.tupleflow.Parameters
import scala.collection.JavaConversions._

/**
 * Created by pat on 7/15/14.
 */
class WikiClueQuery extends  GalagoWrapper
{
  val clueWebIndex = "/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/"
  val wikiIndex = "/mnt/nfs/work1/pat/wikipedia/index"
  val indices = seqAsJavaList(Seq(clueWebIndex, wikiIndex))
  println("Using ClueWeb12 + Wikipedia Index")
  val retrieval = RetrievalFactory.instance(indices, new Parameters)
}

