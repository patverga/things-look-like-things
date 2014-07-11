package co.pemma

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import cc.factorie.app.nlp._
import cc.factorie.util.CmdOptions

object MainThings
{
  val omitArgRegex = "(?:you)|(?:he)|(?:she)|(?:it)|(?:we)|(?:they)|(?:him)|(?:her)|(?:i)|(?:\\W)".r
  val patternRegex = "(?:appear(?:ance is)?|look(?:s|ed)?) (?:exactly |almost| pretty much)?(?:the same as|identical to|similar to|like)".r

  def exportRelationsByThing(thing : String, outputLocation : String)
  {
//    val thingRegex = s"(?:the|a)?${thing}[s]?"
    val regexer = new Regexer(".*", ".*")
    println(patternRegex)
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation)))

//    val queries = regexer.patternList.map(p => s"$thing ${p.replaceAll("\\?", "")}")
//    val documents = GalagoWrapper.runBatchQueries(queries)
    val documents = GalagoWrapper.runQuery(s"$thing looks like", 10000)
    val extractions = RelationExtractor.extractRelations(documents)

    // filter relations that do not involve the 'thing'
    val filteredExtractions = extractions.filter(x => {
      (x._2.arg1.text.contains(thing) || x._2.arg2.text.contains(thing)) &&
        patternRegex.pattern.matcher(x._2.rel.text).matches &&
        !omitArgRegex.pattern.matcher(x._2.arg1.text).matches &&
        !omitArgRegex.pattern.matcher(x._2.arg2.text).matches
    })
    filteredExtractions.foreach(extract =>
    {
      println(s"\n ${extract._3} \n ${extract._1} ${extract._2}")
      writer.println(s"\n ${extract._3} \n ${extract._1} ${extract._2}")
    })
    writer.close()
  }

  def exportRelationsByPattern(query : String, outputLocation : String)
  {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(outputLocation)))

    val documents = GalagoWrapper.runQuery(query, 5000)
    val extractions = RelationExtractor.extractRelations(documents)

    // filter relations that do not match any predefined pattern
    val filteredExtractions = extractions.filter(x => {
      patternRegex.pattern.matcher(x._2.rel.text).matches &&
        !omitArgRegex.pattern.matcher(x._2.arg1.text).matches &&
        !omitArgRegex.pattern.matcher(x._2.arg2.text).matches
    })
    filteredExtractions.foreach(extract =>
    {
      println(s"\n ${extract._3} \n ${extract._1} ${extract._2}")
      writer.println(s"\n ${extract._3} \n ${extract._1} ${extract._2}")
    })
    writer.close()
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
      GalagoWrapper.runQuery(s"${pattern.replaceAll("\\?", "")} $thing")
    })

    // load the data
    var i = 0
    val docSet = documents.toSet[String]
    println("Processing Documents...")
    docSet.foreach(document =>
    {
      val doc = load.LoadPlainText.fromString(document).head
      val sentences = FactorieFunctions.extractSentences(doc)
      regexerObject.extractRegexFromSentences(sentences, thing, output)
      i += 1
      Utilities.printPercentProgress(i, docSet.size)
    })
  }

  def extractContextsBetweenThings(thing1 : String, thing2 :String)
  {
    val regexerObject = new Regexer(thing1, thing2)

    val documents = GalagoWrapper.runQuery(s"$thing1 $thing2")

    val matches = documents.flatMap(doc =>
    {
      val sentences =  FactorieFunctions.extractSentences(load.LoadPlainText.fromString(doc).head)
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
    val snowBall = new CmdOption("snowball", "", "STRING...",  "Google : 'snowball urban dictionary'. You're welcome.")
  }


  def main(args: Array[String])
  {
    println(s"Input Args : ${args.mkString(" ")}")

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
        val output = s"results/pattern/${query.replaceAll("\\s+","-")}.result"
        exportRelationsByPattern(query, output)
      }
    }
    else if(opts.snowBall.wasInvoked) {
      val line = opts.snowBall.value
      val output = s"results/snowball/$line.result"
      SnowBall.run(line, output)
    }
    println("Done.")
  }
}
