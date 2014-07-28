package co.pemma.snowballOrgLoc

import cc.factorie.app.nlp.Sentence
import cc.factorie.app.nlp.ner.NerTag

/**
 * Created by pat on 7/28/14.
 */
class FiveTuple (l : String, t1 : String, c : String, t2 : String, r : String, lf : Boolean)
{
  val left = l
  val tag1 = t1.toUpperCase()
  val center = c
  val tag2 = t2.toUpperCase()
  val right = r
  val locFirst = lf

  override def toString() : String =
  {
    s"$left $tag1 $center $tag2 $right"
  }
}


object FiveTupleFunctions
{
  val window = 5


  def groupTuples(tuples : Seq[FiveTuple])
  {
    val leftCounts = new scala.collection.mutable.HashMap[String, Int]
    val centerCounts = new scala.collection.mutable.HashMap[String, Int]
    val rightCounts = new scala.collection.mutable.HashMap[String, Int]

    tuples.foreach(t =>
    {
      t.left.split(" ").foreach(w =>
      {
        if (counts.contains(w))
          leftCounts.put(w, leftCounts(w) + 1)
        else
          leftCounts.put(w, 1)
      })
      t.center.split(" ").foreach(w =>
      {
        if (counts.contains(w))
          centerCounts.put(w, centerCounts(w) + 1)
        else
          centerCounts.put(w, 1)
      })
      t.right.split(" ").foreach(w =>
      {
        if (rightCounts.contains(w))
          rightCounts.put(w, rightCounts(w) + 1)
        else
          rightCounts.put(w, 1)
      })
    })

    
  }

  def sentenceToFiveTuple(sentence : Sentence) : FiveTuple =
  {
    val it = sentence.iterator
    var tok = it.next()
    var locFirst = false

    val left = new StringBuilder
    var e1StartIndex = 0
    while (it.hasNext && tok.attr[NerTag].baseCategoryValue != "LOC" && tok.attr[NerTag].baseCategoryValue != "ORG") {
      tok = it.next()
      e1StartIndex += 1
    }
    e1StartIndex -= 1

    var wordsAdded = 0
    while(e1StartIndex >= 0 && wordsAdded < window){
      left.append(sentence(e1StartIndex).string)
      left.append(" ")
      wordsAdded += 1
      e1StartIndex -= 1
    }

    val e1 = new StringBuilder
    while (it.hasNext && tok.attr[NerTag].baseCategoryValue == "LOC" || tok.attr[NerTag].baseCategoryValue == "ORG") {
      if (tok.attr[NerTag].baseCategoryValue == "LOC")
        locFirst = true
      e1.append(tok.string)
      e1.append(" ")
      tok = it.next()
    }

    val center = new StringBuilder
    while (it.hasNext && tok.attr[NerTag].baseCategoryValue != "LOC" && tok.attr[NerTag].baseCategoryValue != "ORG") {
      center.append(tok.string)
      center.append(" ")
      tok = it.next()
    }

    val e2 = new StringBuilder
    while (it.hasNext && tok.attr[NerTag].baseCategoryValue == "LOC" || tok.attr[NerTag].baseCategoryValue == "ORG") {
      e2.append(tok.string)
      e2.append(" ")
      tok = it.next()
    }

    val right = new StringBuilder
    var tokens = 0
    while (it.hasNext && tok.attr[NerTag].baseCategoryValue != "LOC" && tok.attr[NerTag].baseCategoryValue != "ORG") {
      if (tokens < window) {
        right.append(tok.string)
        right.append(" ")
        tokens += 1
      }
      tok = it.next()
    }

    // sentence did not contain exactly 2 named entites
    if (it.hasNext)
      return null
    new FiveTuple(left.toString(), e1.toString(), center.toString(), e2.toString(), right.toString(), locFirst)
    //    new FiveTuple(left, e1, center, e2, right, locFirst)
  }
}