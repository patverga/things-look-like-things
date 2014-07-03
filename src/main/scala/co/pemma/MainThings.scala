package co.pemma

import cc.factorie.app.nlp._

object MainThings
{

  var regexerObject : Regexer = null

  def findThingsThatLookLikeThisThingFromFile(thing : String, inputFileLocation : String)
  {
    // load the data
    val source = io.Source.fromFile(inputFileLocation,"ISO-8859-1")
    val doc = load.LoadPlainText.fromSource(source)
    val documentString = processDocument(doc.head).flatMap(_.tokens).toString()
    source.close()

    regexerObject.regex.findAllMatchIn(documentString).foreach(m =>
      println(s"${m.group(0)}")
    )
  }

  def findThingsThatLookLikeThisThingFromGalago(thing : String, output : String)
  {
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

  def extractContextsBetweenThings(arg1 : String, arg2 :String)
  {
    val documents = GalagoClueWeb12.getDocumentsForQueryTerms(s"$arg1 $arg2")

    val matches = documents.flatMap(doc =>
    {
      processDocument(load.LoadPlainText.fromString(doc).head).map(sentence =>
      {
        regexerObject.extractContextsForRelation(arg1, arg2, sentence.string)
      })
    }).flatten

    matches.foreach(m => println(s"${m.group(1)} : ${m.group(2)} : ${m.group(3)}"))
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

    var thing = "whippet"
    if (args.length > 0)
      thing = args(0)

    regexerObject = new Regexer(thing)

        extractContextsBetweenThings("whippet", "greyhound")
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

  }
}
