package co.pemma

import cc.factorie.app.nlp._

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

    io.Source.fromFile("target/classes/patterns").getLines().foreach(pattern =>
    {
      // query galago
      val documents = GalagoClueWeb12.getDocumentsForQueryTerms(s"${pattern.replaceAll("\\?","")} $thing")
      // load the data
      documents.foreach(document =>
      {
        //        Regexer.extractRegexFromString(document, thing, output)
        val doc = load.LoadPlainText.fromString(document).head
        val sentences = processDocument(doc)
        regexerObject.extractRegexFromSentences(sentences, thing, output)
      })
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


  def main(args: Array[String])
  {

    var thing1 = "whippet"
    if (args.length > 0)
      thing1 = args(0)
    var thing2 = "greyhound"
    if (args.length > 1)
      thing2 = args(1)

    extractContextsBetweenThings(thing1, thing2)
    //    val fileLocation = "/home/pat/things-look-like-things/target/classes/wsj/tmp2"
    //    val fileLocation = "/home/pat/things-look-like-things/target/classes/looks-like.data"
    //    findThingsThatLookLikeThisThingFromFile(thing, fileLocation)

    //        val output = s"results/$thing.result"
    //        println(output)
    //        findThingsThatLookLikeThisThingFromGalago(thing, output)

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
