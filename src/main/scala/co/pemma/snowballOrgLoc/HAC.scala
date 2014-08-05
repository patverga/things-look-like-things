package co.pemma.snowballOrgLoc

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack


object HAC
{

  def run(patterns : Seq[FiveTuple]) : Seq[Pattern] =
  {
    println("Clustering Patterns")
    val simMatrix = constructSimilarityMatrix(patterns)
    val clusterMap = cluster(simMatrix)
    centroids(clusterMap, patterns)
  }

  def maxSim(simMat :Array[Array[Double]], remaining : HashSet[Int] ) : (Int, Int, Double) =
  {
    var maxSim = 0.0
    var maxRow = -1
    var maxCol = -1
    var i = 0
    while (i < simMat.length)
    {
      var j = i + 1
      val row = simMat(i)
      while (j < row.length) {

        val sim = row(j)
        if (sim > maxSim) {
          maxSim = sim
          maxRow = i
          maxCol = j
        }
        j += 1
      }
      i += 1
    }
    (maxRow, maxCol, maxSim)
  }

  def centroids(clusterLists : List[scala.collection.mutable.HashSet[Int]], patterns : Seq[FiveTuple]) : Seq[Pattern] =
  {
    val centroids = clusterLists.map(clusterSet =>
    {
      val tuples = clusterSet.map(patterns(_)).toIndexedSeq
      var t = tuples(0)
      var left = t.leftTensor
      var center = t.centerTensor
      var right = t.rightTensor
      var i = 1
      while (i < tuples.size) {
        t = tuples(1)
        left = t.leftTensor
        center = t.centerTensor
        right = t.rightTensor
        i += 1
      }
      left /= tuples.size
      center /= tuples.size
      right /= tuples.size
      new Pattern(left, center, right, t.orgFirst)
    })
    println(s"Clustered ${patterns.size} initial patterns into ${centroids.size} patterns.")
    centroids
  }

  def constructSimilarityMatrix(patterns : Seq[FiveTuple]) : Array[Array[Double]] =
  {
    Array.tabulate(patterns.size, patterns.size)((x, y) => {
      if (x != y) {
        val t1 = patterns(x)
        val t2 = patterns(y)
        if (t1.orgFirst == t2.orgFirst) {
          t1.similarity(t2)
        }
        else
          0
      }
      else 0
    })
  }

  /**
   * Clusters the elements represented as symmetric adjacency matrix.  Values in
   * {@code adj} represent the similarity between any two points using a
   * symmetric similarity metric.  This returns sets of points assigned to the
   * same cluster.
   */
  def cluster(simMat: Array[Array[Double]]) : List[scala.collection.mutable.HashSet[Int]] = {
    // A mapping from cluster id's to their point sets.
    val clusterMap = new HashMap[Int, HashSet[Int]]()
    val remaining = new HashSet[Int]
    // Create a cluster for every data point and add it to the cluster map
    for (r <- 0 until simMat.size) {
      remaining.add(r)
      clusterMap(r) = HashSet(r)
    }

    var done = false
    while (!done)
    {
      val (x, y, sim) = maxSim(simMat, remaining)
//      println(s"$sim $x $y  ${clusterMap.size}")
      if (sim > SnowBall.tauSim) {
        // Remove the current and parent clusters from the cluster map
        // and extract the sizes.
        val (c1Points, c1Size) = removeCluster(clusterMap, x)
        val (c2Points, c2Size) = removeCluster(clusterMap, y)
        val total = c1Size + c2Size

        remaining.remove(y)

        for (i <- remaining) {
          val s1 = simMat(x)(i)
          val s2 = simMat(y)(i)
          val newSim = (c1Size * s1 + c2Size * s2) / total
          simMat(x)(i) = newSim
          simMat(y)(i) = 0
          simMat(i)(x) = newSim
          simMat(i)(y) = 0
        }

        clusterMap(x) = c1Points ++ c2Points
      }
      else done = true
    }

    clusterMap.values.toList
  }

  /**
   * Adds an arbitrary node to the neighbor chain and removes it from the set
   * of remaining clusters.  For simplicity, we just use the most easily
   * accesible element in {@code remaining}.
   */
  def initializeChain(chain: Stack[(Double, Int)], remaining: HashSet[Int]) {
    chain.push((-2.0, remaining.last))
    remaining.remove(remaining.last)
  }

  /**
   * Computes the distance between {@code current} and each cluster in {@code
   * remaining} and returns the cluster with the largest similarity to {@code
   * current}.
   */
  def findBest(remaining: HashSet[Int], adj: Array[Array[Double]], current: Int) =
    remaining.map(i => (adj(current)(i), i)).max

  def removeCluster(clusterMap: HashMap[Int, HashSet[Int]], id: Int) = {
    val s = clusterMap.remove(id).get
    (s, s.size)
  }

}