package co.pemma.snowballOrgLoc

import java.io.File

import cc.factorie.app.nlp.load.LoadOWPL
import cc.factorie.app.nlp.ner.{NerTag, LabeledBilouConllNerTag}
import cc.factorie.app.nlp.{ Token, Sentence}
import co.pemma.OllieExtractor
import co.pemma.Util.ProgressBar

import scala.util.matching.Regex

/**
 * Created by pat on 7/30/14.
 */
object SnowBall
{
  val DIR = "org_loc_sentences"
  val seedTuples = Seq(("google","mountain view"),("microsoft","redmond"),("exxon","irving"),
    ("ibm","armonk"),("boeing","seattle"),("intel","santa clara"))
  val seedRegex = createSeedRegex(seedTuples)
  val seedOrgsRegex = seedTuples.map(_._1).mkString("(?:.*",".*)|(?:.*",".*)").r
  val seedLocsRegex = seedTuples.map(_._2).mkString("(?:.*",".*)|(?:.*",".*)").r
  val simThreshold = .5
  val tupleConfidence = .8

  def main(args: Array[String])
  {
    run()
  }

  def run()
  {
    // read in all nytimes data
    val allData = new File(s"$DIR/utf").listFiles.par.flatMap(f => {
      val fStr = f.toPath.toString
      if (fStr.contains("ny98"))
        readAnnotedData(fStr)
      else
        Seq()
    }).seq

    val map = SnowBall.createWordIndexMap(allData)
    val fiveTuples = FiveTupleFunctions.sentencesToVectors(allData, map)

    // seperate patterns that match seeds and rest
    val partitions = fiveTuples.partition(tuple => {
      seedRegex.pattern.matcher(tuple.entityString).matches
    })

    val otherData = partitions._2.toSet.toSeq
    val patterns = partitions._1
    //    val patterns = HAC.run(partitions._1)
    otherData.foreach(d=>{
      //      if (seedOrgsRegex.pattern.matcher(d.entityString).matches)
      //        println(d.contextString)
    })

    val tuplePatterns = new scala.collection.mutable.HashMap[ExtractedTuple, Seq[ExtractedTuple]]
    // get the extracted tuples that are similar to each pattern
    val patternMatches = patterns.map(p =>
    {
      // must be similar enough and have same tag order (org-loc, or loc-org)
      val matches = fiveTuples.filter(tup => tup.orgFirst == p.orgFirst && tup.similarity(p) >= simThreshold)
      // keep track of each pattern that produces each tuple
      matches.foreach(m => tuplePatterns.put(m, tuplePatterns.getOrElseUpdate(m, Seq()) :+ p))
      (p, matches)
    })

    val patternConfidence = patternMatches.map(pm => {
      println(pm._2.size)
      val posNeg = pm._2.groupBy(m =>
      {
        if (seedOrgsRegex.pattern.matcher(m.entityString).matches())
        {
          if (seedRegex.pattern.matcher(m.entityString).matches())
            "pos"
          else
            "neg"
        }
      }).mapValues(_.length)
      (pm._1, pm._2, posNeg.getOrElse("pos",0).toDouble/(posNeg.getOrElse("neg",0)+1).toDouble)
    })

    patternConfidence.foreach(f => if ( f._3 >= tupleConfidence)
    {
      println(f._1.contextString + "   " + f._2.size)
      f._2.foreach(tu => println(tu.entityString))
    })

    //    similarTuples(patterns, otherData).foreach(tuple =>
    //    {
    //      if (seedOrgsRegex.pattern.matcher(tuple.entityString).matches)
    //      {
    //        tuple.sentence.foreach(t => print(s"(${t.attr[NerTag].categoryValue})${t.string} "))
    //        println(tuple.sentence.string)
    //        println(s"\n${tuple.entityString}\n")
    //      }
    //    })
    //    partitions._1.foreach(p => println(p.contextString))
  }


  def similarTuples(patterns : Seq[Pattern], otherData : Seq[ExtractedTuple]) : Seq[ExtractedTuple] =
  {
    if (patterns != null && patterns.size > 0)
    {
      otherData.filter(dat => {
        val sim = patterns.map(pat => {
          if (dat.orgFirst == pat.orgFirst) {
            val a = dat.similarity(pat)
            //  println(a)
            a
          }
          else
            0
        }).max
        println(sim)
        sim >= simThreshold
      })
    }
    else
      Seq()
  }

  //  def patternConfidence(patterns : Seq[Pattern],otherData : Seq[ExtractedTuple]))

  def seedMatches() : Seq[Sentence] =
  {
    val dir = new File(s"$DIR/utf")
    val sentences = dir.listFiles.par.flatMap(f => {
      val fStr = f.toPath.toString
      if (fStr.contains("ny"))
        readAnnotedData(fStr)
      else
        Seq()
    })
    //        val sentences = readAnnotedData(s"$DIR/lw98.dat")
    //    val co = str.par.filter(coRegex.pattern.matcher(_).matches)
    val matches = sentences.par.filter(s => seedRegex.pattern.matcher(s.string.toLowerCase).matches)
    //    matches.foreach(s => println(s"$s\n"))

    matches.seq
  }

  def createWordIndexMap(sentences : Seq[Sentence]) : scala.collection.mutable.HashMap[String, Int] =
  {
    val map = new scala.collection.mutable.HashMap[String, Int]
    var dex = 0
    sentences.foreach(s =>
    {
      s.tokens.foreach(t =>
      {
        val str = t.string
        if (!map.contains(str))
        {
          map.put(str, dex)
          dex += 1
        }
      })
    })
    map
  }

  def readAnnotedData(inputLoc : String) : Seq[Sentence] =
  {
    println(s"Reading in $inputLoc")
    def nerLabelMaker(tok: Token, labels: Seq[String]) : Seq[LabeledBilouConllNerTag] = {
      Seq(new LabeledBilouConllNerTag(tok, if(labels.size == 0 ) "O" else labels(0)))
    }
    LoadOWPL.fromFilename(inputLoc, nerLabelMaker, "\\s+\\t+\\s+").head.sentences.toSeq.filter(_!="")
  }

  def extractRelations(sentences : Seq[Sentence])
  {
    println(s"Extracting relations from ${sentences.size} sentences.")
    val extractor = new OllieExtractor
    val progress = new ProgressBar(sentences.size)
    val allExtractions = sentences.flatMap(s => {
      progress.increment()
      extractor.extract(s.string.toLowerCase())
    }).filter(e => seedRegex.pattern.matcher(e.toString()).matches())

    println(s" Found ${allExtractions.size} total relations")

    allExtractions.foreach(x => println(x.relation()))
  }

  def createSeedRegex(tuples : Seq[(String, String)]) : Regex =
  {
    tuples.map(t => {
      val c = t._1
      val l = t._2
      s"(?:.*$c.*$l.*)|(?:.*$l.*$c.*)"
    }).mkString("(?:",")|(?:",")").r
  }


}
