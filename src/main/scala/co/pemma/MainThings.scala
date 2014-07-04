package co.pemma

import cc.factorie.app.nlp._
import cc.factorie.util.CmdOptions

object MainThings
{

  def findThingsThatLookLikeThisThingFromFile(thing : String, inputFileLocation : String)
  {
    val regexerObject = new Regexer(thing, ".*")

    // load the data
    val source = io.Source.fromFile(inputFileLocation)
    val doc = load.LoadPlainText.fromSource(source)
    val documentString = processDocument(doc.head).flatMap(_.tokens).toString()
    source.close()

    regexerObject.patternRegex.findAllMatchIn(documentString).foreach(m =>
      println(s"${m.group(0)}")
    )
  }

  def findThingsThatLookLikeThisThingFromGalago(thing : String, output : String)
  {
    val regexerObject = new Regexer(thing, ".*")

    val documents = io.Source.fromFile("target/classes/patterns").getLines().filter(_.startsWith("#")).filterNot(_=="").flatMap(pattern =>
    {
      GalagoClueWeb12.getDocumentsForQueryTerms(s"${pattern.replaceAll("\\?", "")} $thing")
    })
    // load the data
    documents.toSet[String].foreach(document =>
    {
      val doc = load.LoadPlainText.fromString(document).head
      val sentences = processDocument(doc)
      regexerObject.extractRegexFromSentences(sentences, thing, output)
    })
  }

  def extractContextsBetweenThings(thing1 : String, thing2 :String)
  {
    val regexerObject = new Regexer(thing1, thing2)

    val documents = GalagoClueWeb12.getDocumentsForQueryTerms(s"$thing1 $thing2")

    val matches = documents.flatMap(doc =>
    {
      processDocument(load.LoadPlainText.fromString(doc).head).flatMap(sentence =>
      {
        regexerObject.extractContextsForRelation(sentence.string)
      })
    })

    matches.foreach(m => println(s"${m.group(1)}:${m.group(2)}:${m.group(3)}:${m.group(4)}:${m.group(5)}"))
  }

  def processDocument(doc : Document) : Iterable[Sentence] =
  {
    //set up tokenizer / segmenter
    val pipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter))

    // process the document
    print("Processing document...")
    pipeline.process(doc)
    val documentString = doc.sentences
    println("Done")
    documentString
  }

  class ProcessSlotFillingCorpusOpts extends CmdOptions {
    val context = new CmdOption("context", Nil.asInstanceOf[List[String]], "STRING,STRING...", "Takes two strings as inputs then extracts the context surrounding the two things.")
    val looksLike = new CmdOption("looks-like", "", "STRING...", "Takes as input one string and finds things that look like it.")
  }


  def main(args: Array[String])
  {

    val opts = new ProcessSlotFillingCorpusOpts
    opts.parse(args)

    if (opts.context.wasInvoked)
    {
      val thingList = opts.context.value
      extractContextsBetweenThings(thingList(0), thingList(1))
    }
    else if (opts.looksLike.wasInvoked)
    {
      val thing = opts.looksLike.value
      val output = s"results/$thing.result"
      findThingsThatLookLikeThisThingFromGalago(thing, output)
    }


    //    val fileLocation = "/home/pat/things-look-like-things/target/classes/wsj/tmp2"
    //    val fileLocation = "/home/pat/things-look-like-things/target/classes/looks-like.data"
    //    findThingsThatLookLikeThisThingFromFile(thing, fileLocation)



    //            Regexer.testRegexMaker()
    //        JWIWordNetWrap.allThingSynonyms()

    // set file defining patterns
    //    val patternUrl = this.getClass.getResource("/patterns")
    // convert patterns to regex
    //   println(Regexer.generateSurfacePatternRegexes(patternUrl, thing.toLowerCase()).mkString("|").toString())

    //    regexerObject.testContextExtractor()


    println("Done.")
  }
}
