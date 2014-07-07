package co.pemma

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import cc.factorie.app.nlp._
import cc.factorie.util.CmdOptions
import edu.knowitall.ollie.Ollie
import edu.knowitall.ollie.confidence.OllieConfidenceFunction
import edu.knowitall.tool.parse.MaltParser

object MainThings
{
  val pipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter))

  /**
   * given a thing and a file, find all 'looks like' relations involving that thing
   * @param thing
   * @param inputFileLocation
   */
  def findThingsThatLookLikeThisThingFromFile(thing : String, inputFileLocation : String)
  {
    val regexerObject = new Regexer(thing, ".*")

    // load the data
    val source = io.Source.fromFile(inputFileLocation)
    val doc = load.LoadPlainText.fromSource(source).head
    val documentString = pipeline.process(doc).sentences.flatMap(_.tokens).toString()
    source.close()

    regexerObject.patternRegex.findAllMatchIn(documentString).foreach(m =>
      println(s"${m.group(0)}")
    )
  }

  /**
   * given a thing, find all 'looks like' relations involving that thing
   * @param thing
   * @param output
   */
  def findThingsThatLookLikeThisThingFromGalago(thing : String, output : String)
  {
    val regexerObject = new Regexer(thing, ".*")

    val documents = regexerObject.patternList.flatMap(pattern =>
    {
      GalagoClueWeb12.getDocumentsForQueryTerms(s"${pattern.replaceAll("\\?", "")} $thing")
    })

    // load the data
    var i = 0
    val docSet = documents.toSet[String]
    println("Processing Documents...")
    docSet.foreach(document =>
    {
      val doc = load.LoadPlainText.fromString(document).head
      val sentences = pipeline.process(doc).sentences
      regexerObject.extractRegexFromSentences(sentences, thing, output)
      i += 1
      Utilities.printPercentProgress(i, docSet.size)
    })
  }

  /**
   * uses Ollie relation extractor to find all 'looks like' relations from the top 1000 galago results for each pattern
   * @param outputLocation
   */
  def relationsToArgsFromGalago(outputLocation : String)
  {
    val regexerObject = new Regexer(".*", ".*")
    // initialize Ollie
    val parser =  new MaltParser
    val ollie = new Ollie
    val confidence = OllieConfidenceFunction.loadDefaultClassifier()

    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation, true)))

    // get docs from galago
    val documents = regexerObject.patternList.flatMap(pattern =>
    {
      GalagoClueWeb12.getDocumentsForQueryTerms(pattern.replaceAll("\\?", ""))
    })

    // load the data
    var i = 0
    val docSet = documents.filter(_ != "").toSet[String]
    println("Processing Documents...")
    docSet.foreach(document =>
    {
      val doc = load.LoadPlainText.fromString(document).head
      pipeline.process(doc).sentences.foreach(sentence =>
      {
        val sentString = sentence.string
        println(sentString)
        // extract relation from each sentence
        val parsed = parser.dependencyGraph(sentString)
        val extractionInstances = ollie.extract(parsed)
        for (inst <- extractionInstances) {
          val conf = confidence(inst)
          if (inst.extraction.rel.text.matches(regexerObject.patternRegex.toString()))
          {
            println(("%.2f" format conf) + "\t" + inst.extraction)
            writer.println(("%.2f" format conf) + "\t" + inst.extraction)
          }
        }
      })
      i += 1
      Utilities.printPercentProgress(i, docSet.size)
    })
  }

  def extractContextsBetweenThings(thing1 : String, thing2 :String)
  {
    val regexerObject = new Regexer(thing1, thing2)

    val documents = GalagoClueWeb12.getDocumentsForQueryTerms(s"$thing1 $thing2")

    val matches = documents.flatMap(doc =>
    {
      val sentences =  pipeline.process(load.LoadPlainText.fromString(doc).head).sentences
      sentences.flatMap(sentence =>
      {
        regexerObject.extractContextsForRelation(sentence.string)
      })
    })

    matches.foreach(m => println(s"${m.group(1)}:${m.group(2)}:${m.group(3)}:${m.group(4)}:${m.group(5)}"))
  }




  class ProcessSlotFillingCorpusOpts extends CmdOptions {
    val context = new CmdOption("context", Nil.asInstanceOf[List[String]], "STRING,STRING...", "Takes two strings as inputs then extracts the context surrounding the two things.")
    val looksLike = new CmdOption("looks-like", "", "STRING...", "Takes as input one string and finds things that look like it.")
    val ollie = new CmdOption("ollie", "", "",  "Uses Ollie to extract relations for our seed patterns from a galago search of those patterns.")
  }


  def main(args: Array[String])
  {

    val opts = new ProcessSlotFillingCorpusOpts
    opts.parse(args)

    if (opts.context.wasInvoked)
    {
      val thingList = opts.context.value
      extractContextsBetweenThings(thingList(0).toLowerCase(), thingList(1).toLowerCase())
    }
    else if (opts.looksLike.wasInvoked)
    {
      val thing = opts.looksLike.value.toLowerCase()
      val output = s"results/$thing.result"
      findThingsThatLookLikeThisThingFromGalago(thing, output)
    }
    else if (opts.ollie.wasInvoked)
    {
      val output = s"results/ollie.result"
      relationsToArgsFromGalago(output)
    }

    println("Done.")
  }
}
