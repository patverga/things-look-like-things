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

  def main(args: Array[String])
  {
    // test
//    val inputLoc = s"/home/pat/corpus/Na_news98/test"
//    val sentences = readNaNewsData(inputLoc)
//    sentences.foreach(println(_))
//    extractOrgLocSentences(docsToNERSentences(sentences)).foreach(println(_))

    //real
        val inputLoc = args(0)
        val validSentences = extractOrgLocSentences(docsToNERSentences(readNaNewsData(inputLoc)))
        writeAnnotatedSentences(validSentences, inputLoc.split("/").last)
  }

  def readNaNewsData(inputLoc : String) : Seq[String] =
  {
    val source = Source.fromFile(inputLoc, "iso-8859-1")
    println("Loading Docs from file...")
    val lines = source.getLines().toSeq.filter(_!="<p>")

    val lineSplit = lines.mkString("\n").split("<TEXT>").drop(1).map(_.split("</TEXT>")(0))
    val removeStart = lineSplit.map(line =>
    {
      if (line.contains("&md;"))
        line.split("&md;")(1)
      else line
    })
    removeStart
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
        writer.println(s"${t.string.toLowerCase} \t $tag")
      })
      writer.println()
    })
    writer.close()
  }

  def docsToNERSentences( docs : Seq[String]) : Seq[Sentence] =
  {
    println("Converting docs to sentences and tagging NEs")

    val pipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer,
      segment.DeterministicSentenceSegmenter, ner.NoEmbeddingsConllStackedChainNer))

    val progress = new ProgressBar(docs.size)
    docs.flatMap(doc =>
    {
      progress.increment()
      FactorieFunctions.extractSentences(load.LoadPlainText.fromString(doc).head, pipeline)
    }).seq
  }

  def extractOrgLocSentences(sentences : Seq[Sentence]) : Seq[Sentence] =
  {
    println("Keeping sentences that contain both an ORG and a LOC.")
    sentences.filter(s => {
      var org = false
      var loc = false
      s.tokens.foreach(t => {
        val nerTag = t.attr[NerTag]
        if (nerTag.shortCategoryValue == "LOC")
          loc = true
        else if (nerTag.shortCategoryValue == "ORG")
          org = true
      })
      // only keep the sentences that had both an org and a loc
      org && loc
    })
  }
}



