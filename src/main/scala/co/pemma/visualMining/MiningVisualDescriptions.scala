package co.pemma.visualMining

import co.pemma.Util.ProgressBar
import co.pemma.{FactorieFunctions, WikipediaQuery}
import org.lemurproject.galago.core.parse.Document

import scala.collection.GenSeq
import scala.collection.mutable.ListBuffer

/**
 * Created by pat on 8/12/14.
 */
object MiningVisualDescriptions
{
  val galago = new WikipediaQuery
  val docsRetrieved = 500
  val visualModifiers = io.Source.fromURL(this.getClass.getResource("/visual.modifiers")).getLines.filter(_!="").map(_.toLowerCase).toSet
  val things = io.Source.fromURL(this.getClass.getResource("/things-subset")).getLines.map(_.toLowerCase).toList
  val window = 10
  val maxNGram = 5
  val t2 = 1
  val t4 = .0001

  def main(args: Array[String])
  {
    val docMap = things.map(thing => {
      val documents = galago.docsWithTitle(thing, docsRetrieved).seq
      thing -> documents
    }).toMap
    getDocsForThing("whippet", docMap)
  }

  def getDocsForThing(thing : String, docMap : Map[String, Seq[Document]])
  {
    val documents = docMap.getOrElse(thing, Seq())
    println(documents.size)
    documents.foreach(d => println(d.metadata.get("url")))
    val contexts = visualSentences(documents)
    val componentWeigthMap = componentWeights(contexts)
    val progress = new ProgressBar(contexts.size)
    contexts.map(x => {
      progress.increment
      (x , weight4(x, documents, docMap, componentWeigthMap))
    }).filter(_._2 > 0).foreach(c=>{
      println(s"$c ")
    })
  }

  /**
   * @param documents galago docs to be filtered
   * @return sentences that contain a visual modifier
   */
  def visualSentences(documents : GenSeq[Document]) : Seq[(String,String)] =
  {
    val sentences = documents.flatMap(doc => FactorieFunctions.extractSentences(doc.text))
    sentences.flatMap(s => {
      var index = 0
      val tokens = s.tokens
      val result = new ListBuffer[(String,String)]
      while (index < tokens.size) {
        val tok = tokens(index)
        if (visualModifiers.contains(tok.string.toLowerCase())) {
          val modifier = tok.string
          val windEnd = if (index + window < tokens.size) index + window else tokens.size - 1
          val contextTokens = tokens.slice(index+1, windEnd)
          for (ngram <- 1 to maxNGram)
          {
            contextTokens.sliding(ngram).foreach(c => result += Tuple2(modifier, c.map(_.string.replaceAll("[^A-Za-z0-9]", "")).mkString(" ")))
          }
        }
        index += 1
      }
      result
    }).seq
  }

  def componentWeights(contextTupleList : Seq[(String, String)]) : scala.collection.mutable.HashMap[String, Double] =
  {
    val componentVisualModifierMap = new scala.collection.mutable.HashMap[String, ListBuffer[String]]()
    contextTupleList.foreach(x => {
      val list = componentVisualModifierMap.getOrElse(x._2, new ListBuffer[String]())
      list += x._1
      componentVisualModifierMap += x._2 -> list
    })
    val componentWeightMap = new scala.collection.mutable.HashMap[String, Double]()
    componentVisualModifierMap.foreach(x => {
      val weight = if (x._2.size > 1) 1. else 0.
      componentWeightMap += x._1 -> weight
    })
    componentWeightMap
  }

  def weight1(contextTuple : (String, String), documents : Seq[Document]) : Double =
  {
    val str = contextTuple._1 + " " + contextTuple._2
    val score = documents.count(doc => doc.text.contains(str)).toDouble
    score
  }

  def weight2(contextTuple :(String, String), documents : Seq[Document],
              componentWeightMap: scala.collection.mutable.HashMap[String, Double]) : Double =
  {
    weight1(contextTuple, documents) * componentWeightMap.getOrElse(contextTuple._2, 0.0)
  }

  def weight4(contextTuple :(String, String), documents : Seq[Document], docMap : Map[String, Seq[Document]],
              componentWeightMap: scala.collection.mutable.HashMap[String, Double]) : Double =
  {
    val allDocs = docMap.flatMap(_._2)
    val denomStr = contextTuple._1 + " " + contextTuple._2
    val denom = allDocs.count(doc => doc.text.contains(denomStr)).toDouble

    val numRegex = s"${contextTuple._2} (?:is|are) ${contextTuple._1}".r
    val num = allDocs.count(doc => numRegex.pattern.matcher(doc.text).matches()).toDouble

    val score = if (denom == 0) 0
    else if (num/denom > t4) weight2(contextTuple, documents, componentWeightMap) else 0
    score
  }
}
