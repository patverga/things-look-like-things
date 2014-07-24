package co.pemma.snowballOrgLoc

import cc.factorie.app.nlp._
import java.io.{FileWriter, BufferedWriter, PrintWriter, File}

import cc.factorie.app.nlp.load.LoadOWPL
import cc.factorie.app.nlp.ner.{LabeledBilouConllNerTag, NerTag}
import co.pemma.FactorieFunctions
import co.pemma.Util.ProgressBar

import scala.collection.GenSeq
import scala.io.Source

/**
 * Created by pat on 7/24/14.
 */
object ReadAndEntityTag
{

  //  val seeds = Seq()


  def main(args: Array[String])
  {
    val file = "test"
    //    val file = "lw97.dat"

//    val inputLoc = s"/home/pat/corpus/Na_news98/$file"
//    val validSentences = extractOrgLocSentences(docsToNERSentences(readNaNewsData(inputLoc)))
//    writeSentences(validSentences, file)
//
//    readAnnotedData(file).foreach(s => {
//      s.tokens.foreach(t => {
//        println(t.string +" \t " + t.attr[NerTag].categoryValue)
//      })
//    })

    val sentences = readAnnotedData(file)

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

  def readAnnotedData(fileName : String) : Seq[Sentence] =
  {
    val inputLoc = s"org_loc_sentences/$fileName"
    def nerLabelMaker(tok: Token, labels: Seq[String]) : LabeledBilouConllNerTag = {
      new LabeledBilouConllNerTag(tok, if(labels.size == 1) "O" else labels(1))
    }
    LoadOWPL.fromFilename(inputLoc, nerLabelMaker).head.sentences.toSeq
  }

  def docsToNERSentences( docs : Seq[String]) : GenSeq[Sentence] =
  {
    println("Converting Docs to Sentences and Tagging NERs")

    val pipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer,
      segment.DeterministicSentenceSegmenter, ner.NoEmbeddingsConllStackedChainNer))

    val progress = new ProgressBar(docs.size)
    docs.flatMap(doc =>
    {
      progress.increment()
      FactorieFunctions.extractSentences(load.LoadPlainText.fromString(doc).head, pipeline)
    })
  }

  def extractOrgLocSentences(sentences : GenSeq[Sentence]) : GenSeq[Sentence] =
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

  def writeSentences(sentences : GenSeq[Sentence], file : String)
  {
    println(s"Exporting valid sentences to file : $file")
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(s"org_loc_sentences/$file")))
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
}
