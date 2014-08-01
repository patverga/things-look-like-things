package co.pemma.snowballOrgLoc

import cc.factorie.app.nlp.{TokenSpan, Sentence}
import cc.factorie.app.nlp.ner.NerTag
import cc.factorie.app.nlp.phrase.{PhraseList, Phrase}
import cc.factorie.la.SparseTensor1

/**
 * Created by pat on 7/28/14.
 */

class FiveTuple(t : SparseTensor1, c : Seq[TokenSpan], e : Seq[Phrase], s : Sentence)
{
  val tensor = t
  val contexts = c
  val entities = e
  val sentence = s
  val orgFirst = entities(0).headToken.attr[NerTag].shortCategoryValue == "ORG"
}

object FiveTupleFunctions
{
  val window = 3
  val distance = 5

  val weightSides = .2
  val weightCenter = .6

  def extractPhrases(sentence : Sentence)
  {
    val phrases = scala.collection.mutable.ArrayBuffer[Phrase]()
    var i = 0
    while (i < sentence.length) {
      val tag = sentence(i).nerTag
      if (tag.categoryValue != "O" &&
        (tag.shortCategoryValue == "ORG" || tag.shortCategoryValue == "LOC")
      )
      {
        val entitySentenceStart = i
        var j = i + 1
        if (!tag.categoryValue.startsWith("U")) {
          while (j < sentence.length &&
            (sentence(j).nerTag.categoryValue.startsWith("I") || sentence(j).nerTag.categoryValue.startsWith("L"))) {
            j += 1
          }
        }
        val entityLen = j-i
        i = j

        /* we have the entityStart idx wrt this sentence, now we need to get it wrt this section */
        val entitySectionStart = entitySentenceStart + sentence.start

        val ts = new TokenSpan(sentence.section, entitySectionStart, entityLen)
        val phrase = new Phrase(sentence.section, entitySectionStart, entityLen, ts.length-1)
        phrases += phrase
      } else {
        i += 1
      }
    }
    sentence.attr += new PhraseList(phrases)
  }

  def sentenceToRelationContext(sentence : Sentence, map : scala.collection.mutable.HashMap[String, Int])
  : Seq[FiveTuple] =
  {
    val result = scala.collection.mutable.ArrayBuffer[FiveTuple]()
    val phrases = sentence.attr[PhraseList]
    var i = 0
    while (i < phrases.size)
    {
      val p1 = phrases(i)
      var j = i + 1
      while (j < phrases.size)
      {
        val p2 = phrases(j)
        // only consider when one phrase is an org and one is a loc
        if (p1.headToken.attr[NerTag].shortCategoryValue != p2.headToken.attr[NerTag].shortCategoryValue &&
          // and p1 and p2 are close enough together
          (p2.headToken.positionInSection - p1.headToken.next(p1.length).positionInSection) <= distance)
        {
          //          println(s"${p1.string}   ${p2.string}")
          val context = contextBetweenPhrases(p1, p2, sentence)
          val tensor = contextsToVector(context._1, map)
          result += new FiveTuple(tensor, context._1, context._2, sentence)
        }
        j += 1
      }
      i += 1
    }
    result.seq
  }

  def contextBetweenPhrases(p1 : Phrase, p2 : Phrase, sentence : Sentence) :
  (Seq[TokenSpan],Seq[Phrase]) =
  {
    val p1Start = p1.headToken.prev(p1.length).positionInSentence
    val left =
      if (p1Start - window > sentence.start)
        new TokenSpan(sentence.section, (p1Start + sentence.start+1)-window, window)
      else
        new TokenSpan(sentence.section, sentence.start, 0)


    val p1End = p1.headToken.positionInSentence + sentence.start
    val p2Start = p2.headToken.prev(p2.length).positionInSentence + sentence.start
    val center =
      if (p1End >= p2Start)
        new TokenSpan(sentence.section, sentence.start, 0)
      else
        new TokenSpan(sentence.section, p1End+1, p2Start-p1End)

    val p2End = p2.headToken.positionInSentence + sentence.start
    val right =
      if (p2End + window < sentence.end)
        new TokenSpan(sentence.section, p2End+1, window)
      else
        new TokenSpan(sentence.section, p2End+1, sentence.end-(p2End+1))

    //    println("\n"+sentence)
    //    println(left.string)
    //    println(p1.string)
    //    println(center.string)
    //    println(p2.string)
    //    println(right.string)
    (Seq(left, center, right),Seq(p1, p2))
  }

  def contextsToVector(contexts : Seq[TokenSpan], map : scala.collection.mutable.HashMap[String, Int])
  : SparseTensor1 =
  {
    // iterate over the three contexts
    val tensor = new SparseTensor1(map.size * 3)
    var startDex = 0
    contexts.foreach(context =>
    {
      // weight contexts differently
      val weight =
        if (startDex == map.size) weightCenter else weightSides

      // iterate over each token
      context.foreach(tok =>
      {
        val dex = map.getOrElse(tok.string,-1)
        tensor(dex + startDex) += (1/context.length) * weight
      })
      startDex += map.size
    })
    tensor
  }

  def sentencesToVectors(sentences : Seq[Sentence], map : scala.collection.mutable.HashMap[String, Int])
  : Seq[FiveTuple] =
  {
    sentences.flatMap(s => {
      extractPhrases(s)
      sentenceToRelationContext(s, map)
    })
  }

  def main(args: Array[String])
  {
//    val inputLoc = s"org_loc_sentences/test"
//
//    val sentences = SnowBall.readAnnotedData(inputLoc)
//    val map = SnowBall.createWordIndexMap(sentences)
//    sentences.foreach(s => {
//      extractPhrases(s)
//      val contexts = sentenceToRelationContext(s, map)
//      val i = 1
      //      contextsToVector(contexts, map)
      //          s.attr[PhraseList].foreach(t => {
      ////            println(t.string +" \t " + t.attr[NerTag].categoryValue)
      //            println(t)
      //          })
//    })
  }
}