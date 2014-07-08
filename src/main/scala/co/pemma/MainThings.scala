package co.pemma

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import cc.factorie.app.nlp._
import cc.factorie.util.CmdOptions
import edu.knowitall.ollie.OllieExtraction

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
      GalagoWrapper.getDocumentsForQueryTerms(s"${pattern.replaceAll("\\?", "")} $thing")
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

  def exportRelationsByThing(thing : String, outputLocation : String)
  {
    val extractions = relationsWithThingFromGalago(thing)

    // filter relations that do not involve the 'thing'
    val filteredExtractions = extractions.filter(x => (x._2.arg1.text.contains(thing) || x._2.arg2.text.contains(thing)) )
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation, true)))
    filteredExtractions.foreach(extract =>
    {
      println(s"${extract._1} ${extract._2}")
      writer.println(s"${extract._1} ${extract._2}")
    })
    writer.close()
  }

  def exportRelationsByPattern(query : String, outputLocation : String)
  {
    val patternRegex = new Regexer(".*", ".*").patternList.mkString("(?:.*",".*)|(?:.*",")")
    val omitArgRegex = "(?:you)|(?:he)|(?:she)|(?:it)|(?:we)|(?:they)|(?:him)|(?:her)|(?:i)|(?:\\W)"

    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation, true)))

    val extractions = relationsWithThingFromGalago(query)
    // filter relations that do not match any predefined pattern
    val filteredExtractions = extractions.filter(x =>
      (x._2.rel.text.matches(patternRegex) &&
        x._2.arg1.text.matches(omitArgRegex) &&
        x._2.arg2.text.matches(omitArgRegex))
    )
    filteredExtractions.foreach(extract =>
    {
      println(s"${extract._1} ${extract._2}")
      writer.println(s"${extract._1} ${extract._2}")
    })
    writer.close()
  }

  /**
   * given a query, find all relations from the documents
   */
  def relationsWithThingFromGalago(query : String) : Iterable[(String, OllieExtraction)] =
  {
    val documents = GalagoWrapper.getDocumentsForQueryTerms(query)
    // load the data
    var i = 0
    println("Processing Documents...")
    val allExtractions = documents.flatMap(document =>
    {
      i += 1
      Utilities.printPercentProgress(i, documents.size)

      val doc = load.LoadPlainText.fromString(document).head
      val sentences = pipeline.process(doc).sentences
      val extractions = sentences.filter(s => {s.string != "" && s.string != null && s.string.length > 5 }).flatMap(sentence =>
      {
          NLPThings.ollieExtraction(sentence.string.toLowerCase())
      }).filter(_ != null)
      extractions
    })
    allExtractions
  }

  def extractContextsBetweenThings(thing1 : String, thing2 :String)
  {
    val regexerObject = new Regexer(thing1, thing2)

    val documents = GalagoWrapper.getDocumentsForQueryTerms(s"$thing1 $thing2")

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
    val thing = new CmdOption("thing", "", "STRING...", "Takes as input one string and finds things that look like it.")
    val pattern = new CmdOption("pattern", "", "STRING...",  "Uses Ollie to extract relations for our seed patterns from a galago search of those patterns.")
  }


  def main(args: Array[String])
  {

    val opts = new ProcessSlotFillingCorpusOpts
    opts.parse(args)

    if (opts.context.wasInvoked)
    {
      val thingList = opts.context.value
      extractContextsBetweenThings(thingList(0).toLowerCase, thingList(1).toLowerCase)
    }
    else if (opts.thing.wasInvoked)
    {
      val thing = opts.thing.value.toLowerCase
      val output = s"results/thing/$thing.result"
      exportRelationsByThing(thing, output)
    }
    else if (opts.pattern.wasInvoked) {
      val query = opts.pattern.value.toLowerCase.replaceAll("\\?","")
      if (!query.startsWith("#") && query != "") {
        val output = s"results/pattern/$query.result"
        exportRelationsByPattern(query, output)
      }
    }

    println("Done.")
  }
}
