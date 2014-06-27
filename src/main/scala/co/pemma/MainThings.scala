package co.pemma

import cc.factorie.app.nlp._

object MainThings
{
  def findThingsThatLookLikeThisThing(thing : String)
  {
    //        val fileLocation = "/home/pat/things-look-like-things/target/classes/wsj/tmp"
    val fileLocation = "/home/pat/things-look-like-things/target/classes/" + thing + ".data"
    val documentString = processDocument(fileLocation)

    val lowerCaseThing = thing.toLowerCase()

    // set file defining patterns
    val patternUrl = this.getClass.getResource("/patterns")
    // convert patterns to regex
    val regexList = Regexer.generateSurfacePatternRegexes(patternUrl, lowerCaseThing)

    println("Looking for things that look like " + thing)
    documentString.foreach( sentence =>
    {
      val lowerSentence = sentence.toLowerCase
      regexList.foreach( regex =>
      {
        //          println(regex)
        regex.findAllMatchIn(lowerSentence).foreach( matches =>
        {
          println(matches.group(1), matches.group(2), matches.group(3))
        })
      })
    })

  }

  def processDocument(fileLocation : String) : Iterable[String] =
  {
    // load the data
    val source = io.Source.fromFile(fileLocation,"ISO-8859-1")
    val doc = load.LoadPlainText.fromSource(source)

    //set up tokenizer / segmenter
    val pipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter))

    // process the document
    print("Processing data...")
    pipeline.process(doc.head)

    val documentString = doc.head.sentences.map(_.tokens.map(_.string)).map(_.mkString(" "))
    source.close()
    println("done processing")

    documentString
  }


  def main(args: Array[String])
  {
//    Regexer.testRegexMaker()
//        findThingsThatLookLikeThisThing("Actor")
        JWIWordNetWrap.getSynonyms("dog").foreach(s => println(s))

  }
}
