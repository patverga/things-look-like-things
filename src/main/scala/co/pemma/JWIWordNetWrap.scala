package co.pemma

import edu.mit.jwi.item.POS
import edu.mit.jwi.{Dictionary}

/**
 * Created by pat on 6/25/14.
 */
object JWIWordNetWrap
{
  // construct the URL to the Wordnet dictionary directory
  val WORDNETHOME = "/dict"

  def getSynonyms(inputWord : String) : collection.mutable.MutableList[String] =
  {
    val dict = new Dictionary(this.getClass.getResource(WORDNETHOME))
    dict.open ()

    // look up first sense of the word " dog "
    val idxWord = dict.getIndexWord(inputWord, POS.NOUN )
    val wordID = idxWord.getWordIDs().get(0)
    // 1 st meaning
    val word = dict.getWord(wordID)
    val words = word.getSynset.getWords
    // iterate over words associated with the synset

    var i = 0
    val lemmas = collection.mutable.MutableList[String]()
    while (i < (words.size()))
    {
      lemmas += words.get(i).getLemma
      i += 1
    }
    lemmas
  }

  def testDictionary ()
  {
    // construct the dictionary object and open it
    val dict = new Dictionary(this.getClass.getResource(JWIWordNetWrap.WORDNETHOME))
    dict.open ()
    // look up first sense of the word "dog"
    val idxWord = dict.getIndexWord("dog", POS.NOUN )
    val wordID = idxWord.getWordIDs().get(0)
    val word = dict . getWord( wordID )
    println( " Id = " + wordID)
    println( " Lemma = " + word.getLemma())
    println( " Gloss = " + word.getSynset().getGloss())
  }
}
