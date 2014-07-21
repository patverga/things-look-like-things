package co.pemma.mineWikipedia

import cc.factorie.app.nlp.lexicon.StopWords
import co.pemma.galagos.WikipediaQuery
import org.jsoup._

/**
 * Created by pat on 7/17/14.
 */
object WikiMiner
{

  def getWikiForThing(thing : String)
  {
    val wiki = new WikipediaQuery
//    val doc = wiki.runQuery()
  }

  def extractSection(doc : String, section : String) {
    val startRegex = ("\\s*<h[1-3].*id=\""+section+"\".*").r
    val endRegex = ("\\s*<h[1-3].*id=\"(?!"+section+").*").r

    val lines = doc.split("\n")

    var dex = 0
    while (dex < lines.length && !startRegex.pattern.matcher(lines(dex)).matches())
      dex += 1
    dex += 1
    while (dex < lines.length && !endRegex.pattern.matcher(lines(dex)).matches())
    {
      val line = lines(dex).trim
      if (line.startsWith("<p>"))
        println(line.replaceAll("<[^>]+>|&nbsp;|&quot|(?:\\Q[0-9]\\E)*", ""))
      dex += 1
    }
  }

  def extractAppearanceSection(doc : String) {
   extractSection(doc, "Appearance")
  }

  def extractFirstSection(doc : String) {
    extractSection(doc, "firstHeading")
  }

  def extractParts(doc : String)
  {
    extractFirstSection(doc)
    extractAppearanceSection(doc)
  }

  def removeStopWords(text : String)
  {
    StopWords.contains("t")

  }


  def main(args: Array[String]) {
    extractParts(Jsoup.connect("http://en.wikipedia.org/wiki/Whippet").get().toString)
  }
}
