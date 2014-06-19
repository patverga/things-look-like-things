import cc.factorie.app.nlp._

import scala.util.matching.Regex

object MainButt extends App
{

  // load the data
  val doc = load.LoadPlainText.fromSource(io.Source.fromString("This is a poop string sentence. Butts can't poop's butts :) butts butts anuus...\n"))
  // set up tokenizer / segmenter
  val anuusPoopline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter))

  // process the document
  anuusPoopline.process(doc.head)
  // print out the individual sentences in the document
  val sentenceString = doc.head.sentences.map(_.tokens.map(_.string)).map(_.mkString(" "))
  //sentenceString.foreach(sentence => println(sentence.mkString(" ")))

  // set file defining patterns
  val patternUrl = this.getClass.getResource("/patterns")
  // convert patterns to regex
  val regexMap = generateSurfacePatternRegexesFromURL(patternUrl, patternUrl)

  // dont compile
  regexMap.foreach{case(name,regexList) =>
    regexList.foreach( regex =>
      regex.findAllMatchIn(sentenceString.head).foreach(matches => {
    println(matches.group(0))
  }))}

  def generateSurfacePatternRegexesFromURL(patternListURL: java.net.URL, typeListURL: java.net.URL): collection.mutable.Map[String, collection.mutable.ArrayBuffer[Regex]] =
  {
    val types = io.Source.fromURL(typeListURL).getLines().filter(_ != "").filter(!_.startsWith("#"))
    //val types = Seq("PER","ORG","LOC")

    //    println("Generating surface pattern regexes")

    val patternMap = collection.mutable.Map[String, collection.mutable.ArrayBuffer[Regex]]()

    val whitespaceRegex = """\s+""".r
    val argRegexString = "\\$ARG[0-2]"
    //    val typedArgRegexString = types.mkString("((?😞?:", "\\|", ")\\\\s?)+)")
    val typeString = types.mkString("(?:", "|", ")")

    //    val typedArgRegexString = s"((?:$typeString\\\\s)*$typeString)(?!\\\\s$typeString)"
    val typedArgRegexString = s"($typeString(?:_$typeString)*)" //(?!\\\\s$typeString)"
    //    "($typeString(?😕\s$typeString)*)".r

    io.Source.fromURL(patternListURL).getLines().foreach(line => {
      if (!line.startsWith("#") && line != "") {
        val splitLine = whitespaceRegex.split(line)
        val patternType = splitLine.head

        val rest = splitLine.drop(1).mkString(" ")
          .replaceAllLiterally(" *", "(?: \\S+){0,3}") // handle wildcards for words
          .replaceAllLiterally("*", "\\S+") // handle wildcards within a word
          .replaceAllLiterally("(", "\\(") // handle literal parens
          .replaceAllLiterally(")", "\\)") // handle literal parens
          .replaceAll(argRegexString, typedArgRegexString) // handle typed args

        //        println(s"pattern: $rest")

        val pattern = rest.r
        patternMap.getOrElseUpdate(patternType, collection.mutable.ArrayBuffer[Regex]()) += pattern
      }
    })
    patternMap
  }
}
