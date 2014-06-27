package co.pemma

import cc.factorie.app.nlp._

object MainThings
{

  def findThingsThatLookLikeThisThingFromFile(thing : String, fileLocation : String)
  {
    // load the data
    val source = io.Source.fromFile(fileLocation,"ISO-8859-1")
    val doc = load.LoadPlainText.fromSource(source)
    val documentString = processDocument(doc)
    source.close()
    Regexer.extractRegexFromString(documentString, thing)
  }

  def findThingsThatLookLikeThisThingFromGalago(thing : String)
  {
    val documents = GalagoClueWeb12.getDocumentsForQueryTerms("looks like " + thing)
    // load the data
    documents.foreach(document =>
    {
      val doc = load.LoadPlainText.fromString(document)
      val documentString = processDocument(doc)
      Regexer.extractRegexFromString(documentString, thing)
    })
  }

  def processDocument(doc : Seq[Document]) : Iterable[cc.factorie.app.nlp.Sentence] =
  {
    //set up tokenizer / segmenter
    val pipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter))

    // process the document
    print("Processing data...")
    pipeline.process(doc.head)

    //    val documentString = doc.head.string
    val documentString = doc.head.sentences //.map(_.tokens.map(_.string)).map(_.mkString(" ")).toString()
    println("done processing")

    documentString
  }


  def main(args: Array[String])
  {
//    //    val fileLocation = "/home/pat/things-look-like-things/target/classes/wsj/tmp2"
//    val fileLocation = "/home/pat/things-look-like-things/target/classes/looks-like.data"
//    findThingsThatLookLikeThisThingFromFile("", fileLocation)

    findThingsThatLookLikeThisThingFromGalago("whippet")

    //    Regexer.testRegexMaker()
    //    JWIWordNetWrap.allThingSynonyms()


  }
}
