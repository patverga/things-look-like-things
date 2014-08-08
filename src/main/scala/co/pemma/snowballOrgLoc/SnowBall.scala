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
  val seedRegex = createSeedRegex()
  val simThreshold = .2

  def main(args: Array[String])
  {
    run()
  }

  def run()
  {
    // read in all nytimes data
    val allData = new File(s"$DIR/utf").listFiles.par.flatMap(f => {
      val fStr = f.toPath.toString
      if (fStr.contains("ny98ae"))
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

    val otherData = partitions._2
    val patterns = HAC.run(partitions._1)
    //    otherData.foreach(d=> println(d.contexts(1).string))

    similarTuples(patterns, otherData).foreach(tuple =>
    {
      //      tuple.sentence.foreach(t=>print(s"(${t.attr[NerTag].categoryValue})${t.string} "))
//      println(tuple.sentence.string)
//      println(s"\n${tuple.entityString}\n")
    })

    partitions._1.foreach(p => println(p.contextString))

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

  def createSeedRegex() : Regex =
  {
    val tuples = Seq(("google","mountain view"),("microsoft","redmond"),("exxon","irving"),
      ("ibm","armonk"),("boeing","seattle"),("intel","santa clara"))

    tuples.map(t => {
      val c = t._1
      val l = t._2
      s"(?:.*$c.*$l.*)|(?:.*$l.*$c.*)"
    }).mkString("(?:",")|(?:",")").r
  }


}
