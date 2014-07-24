package co.pemma.snowballOrgLoc

import cc.factorie.app.nlp._
import java.io.{FileWriter, BufferedWriter, PrintWriter, File}

import cc.factorie.app.nlp.load.LoadOWPL
import cc.factorie.app.nlp.ner.{LabeledBilouConllNerTag, NerTag}
import co.pemma.{OllieExtractor, FactorieFunctions}
import co.pemma.Util.ProgressBar

import scala.collection.GenSeq
import scala.io.Source

/**
 * Created by pat on 7/24/14.
 */
object ReadAndEntityTag
{
  val DIR = "org_loc_sentences"
  //  val seeds = Seq()


  def main(args: Array[String])
  {
//    val file = "test"
//    val file = "lw97.dat"
//    val inputLoc = s"/home/pat/corpus/Na_news98/$file"

    val inputLoc = args(0)

    val validSentences = extractOrgLocSentences(docsToNERSentences(readNaNewsData(inputLoc)))
    writeAnnotatedSentences(validSentences, inputLoc.split("/").last)

//    readAnnotedData(file).foreach(s => {
//      s.tokens.foreach(t => {
//        println(t.string +" \t " + t.attr[NerTag].categoryValue)
//      })
//    })

//    val sentences = readAnnotedData(file)
//    extractRelations(sentences)

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
    val inputLoc = s"$DIR/$fileName"
    def nerLabelMaker(tok: Token, labels: Seq[String]) : LabeledBilouConllNerTag = {
      new LabeledBilouConllNerTag(tok, if(labels.size == 1) "O" else labels(1))
    }
    LoadOWPL.fromFilename(inputLoc, nerLabelMaker).head.sentences.toSeq.filter(_!="")
  }

  def writeAnnotatedSentences(sentences : GenSeq[Sentence], file : String)
  {
    println(s"Exporting valid sentences to file : $file")
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(s"$DIR/$file")))
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

  def docsToNERSentences( docs : Seq[String]) : GenSeq[Sentence] =
  {
    println("Converting Docs to Sentences and Tagging NERs")

    val pipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer,
      segment.DeterministicSentenceSegmenter, ner.NoEmbeddingsConllStackedChainNer))

    val progress = new ProgressBar(docs.size)
    docs.par.flatMap(doc =>
    {
//      progress.increment()
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

  def extractRelations(sentences : Seq[Sentence])
  {
    println(s"Extracting relations")
    val extractor = new OllieExtractor
    val progress = new ProgressBar(sentences.size)
    val allExtractions = sentences.flatMap(s => {
      progress.increment()
      extractor.extract(s)
    })
    println(s" Found ${allExtractions.size} total relations")

    allExtractions.foreach(x => println(x.relation()))
  }
}
