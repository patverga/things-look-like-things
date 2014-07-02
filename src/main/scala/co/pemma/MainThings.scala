package co.pemma

import cc.factorie.app.nlp._

object MainThings
{

  def findThingsThatLookLikeThisThingFromFile(thing : String, inputFileLocation : String)
  {
    // load the data
    val source = io.Source.fromFile(inputFileLocation,"ISO-8859-1")
    val doc = load.LoadPlainText.fromSource(source)
    val documentString = processDocument(doc.head).flatMap(_.tokens).toString()
    source.close()
    //    Regexer.extractRegexFromString(documentString, thing)
  }

  def findThingsThatLookLikeThisThingFromGalago(thing : String, output : String)
  {
    io.Source.fromFile("/home/pat/things-look-like-things/target/classes/patterns").getLines().foreach(pattern =>
    {
      // query galago
      val documents = GalagoClueWeb12.getDocumentsForQueryTerms(s"${pattern.replaceAll("\\?","")} $thing")
      // load the data
      documents.foreach(document =>
      {
        Regexer.extractRegexFromString(document, thing, output)
        //        val doc = load.LoadPlainText.fromString(document).head
        //        val sentences = processDocument(doc)
        //        if (sentences != null)
        //          Regexer.extractRegexFromSentences(sentences, thing, output)
      })
    })
  }

  def processDocument(doc : Document) : Iterable[Sentence] =
  {
    //set up tokenizer / segmenter
    val pipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter))

    // process the document
    print("Processing data...")
    try
    {
      pipeline.process(doc)
      val documentString = doc.sentences
      println("done processing")
      documentString
    } catch {
      case e: Exception => return (null)
    }
  }


  def main(args: Array[String])
  {

    var thing = "whippet"
    if (args.length > 0)
      thing = args(0)

    //    val fileLocation = "/home/pat/things-look-like-things/target/classes/wsj/tmp2"
    //        val fileLocation = "/home/pat/things-look-like-things/target/classes/looks-like.data"
    //        findThingsThatLookLikeThisThingFromFile(thing, fileLocation)

    val output = s"results/$thing.result"
    println(output)
    findThingsThatLookLikeThisThingFromGalago(thing, output)

    //        Regexer.testRegexMaker()
    //        JWIWordNetWrap.allThingSynonyms()

  }
}
