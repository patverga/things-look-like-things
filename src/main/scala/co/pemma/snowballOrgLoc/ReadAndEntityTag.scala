package co.pemma.snowballOrgLoc

import cc.factorie.app.nlp._
import java.io.{FileWriter, BufferedWriter, PrintWriter, File}
import util.control.Breaks._

import cc.factorie.app.nlp.load.LoadOWPL
import cc.factorie.app.nlp.ner.{LabeledBilouConllNerTag, NerTag}
import co.pemma.{ClauseIEExtractor, ReverbExtractor, OllieExtractor, FactorieFunctions}
import co.pemma.Util.ProgressBar

import scala.collection.GenSeq
import scala.io.Source
import scala.util.matching.Regex

/**
 * Created by pat on 7/24/14.
 */
object ReadAndEntityTag
{
  val DIR = "org_loc_sentences"
   val seedRegex = createSeedRegex()
  println(seedRegex)

  def main(args: Array[String])
  {
    //    val file = "test"
//    val file = "lw97.dat"
//        val inputLoc = s"/home/pat/corpus/Na_news98/$file"

        val inputLoc = args(0)
    //
        val validSentences = extractOrgLocSentences(docsToNERSentences(readNaNewsData(inputLoc)))
        writeAnnotatedSentences(validSentences, inputLoc.split("/").last)

    //    readAnnotedData(file).foreach(s => {
    //      s.tokens.foreach(t => {
    //        println(t.string +" \t " + t.attr[NerTag].categoryValue)
    //      })
    //    })

    //    extractRelations(sentences)

//    seedMatches().foreach(s => println(s"$s\n"))
//    docsToNERSentences(readNaNewsData(inputLoc)).foreach(s => println(s"$s\n"))
//    extractRelations(seedMatches())
//    FiveTupleFunctions.groupTuples(seedMatches().map(s =>{
//      FiveTupleFunctions.sentenceToFiveTuple(s)
//    }).filter(_!=null))
  }

  def seedMatches() : Seq[Sentence] =
  {
    val dir = new File(s"DIR/utf")
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

  def readNaNewsData(inputLoc : String) : Seq[String] =
  {
    val source = Source.fromFile(inputLoc, "iso-8859-1")
    println("Loading Docs from file...")
    val lines = source.getLines().toSeq
    val uselessTagRegex = "(?:<[A-Z].*>)|(?:</(?!DOC).*>)".r

    val filteredLines = lines.filter(!uselessTagRegex.pattern.matcher(_).matches())
    val docs = filteredLines.mkString("\n").split("</DOC>")

    println(s"${lines.size} lines : ${filteredLines.size} filtered Lines : ${docs.size} docs")
    docs.toSeq
  }

  def readAnnotedData(inputLoc : String) : Seq[Sentence] =
  {
    println(s"Reading in $inputLoc")
    def nerLabelMaker(tok: Token, labels: Seq[String]) : Seq[LabeledBilouConllNerTag] = {
      Seq(new LabeledBilouConllNerTag(tok, if(labels.size == 0 ) "O" else labels(0)))
    }
    LoadOWPL.fromFilename(inputLoc, nerLabelMaker, "\\s+\\t+\\s+").head.sentences.toSeq.filter(_!="")
  }

  def writeAnnotatedSentences(sentences : GenSeq[Sentence], file : String)
  {
    println(s"Exporting valid sentences to file : $file")
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(s"$DIR/original/$file")))
    sentences.foreach(sentence =>
    {
      sentence.tokens.foreach(t => {
        val tag =
          if (t.attr[NerTag].categoryValue == "O")  ""
          else t.attr[NerTag].categoryValue
        //        if (tag.equals("O")) tag = ""
        writer.println(s"${t.string} \t $tag")
      })
      writer.println()
    })
    writer.close()
  }

  def docsToNERSentences( docs : Seq[String]) : Seq[Sentence] =
  {
    println("Converting Docs to Sentences and Tagging NERs")

    val pipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer,
      segment.DeterministicSentenceSegmenter, ner.NoEmbeddingsConllStackedChainNer))

    val progress = new ProgressBar(docs.size)
    docs.flatMap(doc =>
    {
      //      progress.increment()
      FactorieFunctions.extractSentences(load.LoadPlainText.fromString(doc).head, pipeline)
    })
  }

  def extractOrgLocSentences(sentences : Seq[Sentence]) : Seq[Sentence] =
  {
    println("Keeping sentences that contain both an ORG and a LOC.")
    sentences.filter(s => {
      var org = false
      var loc = false
      s.tokens.foreach(t => {
        val nerTag = t.attr[NerTag]
        if (nerTag.baseCategoryValue == "LOC")
          loc = true
        else if (nerTag.baseCategoryValue == "ORG")
          org = true
      })
      // only keep the sentences that had both an org and a loc
      org && loc
    })
  }

  def extractRelations(sentences : Seq[Sentence])
  {
    println(s"Extracting relations from ${sentences.size} sentences.")
    val extractor = new ClauseIEExtractor
    val progress = new ProgressBar(sentences.size)
    val allExtractions = sentences.flatMap(s => {
      progress.increment()
      extractor.extract(s.string.toLowerCase())
    }).filter(e => seedRegex.pattern.matcher(e.toString()).matches())

    println(s" Found ${allExtractions.size} total relations")

    allExtractions.foreach(x => println(x.relation()))
  }
}



