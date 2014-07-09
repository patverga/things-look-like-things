package co.pemma

import cc.factorie.app.nlp.lemma.PorterLemmatizer
import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.{ISynsetID, POS, Pointer}

/**
 * Created by pat on 6/25/14.
 */
object JWIWordNetWrap {
  // construct the URL to the Wordnet dictionary directory
  val WORDNET_HOME = "/dict"
  val THING_LIST_FILE = "/things"
  val dict = new Dictionary(this.getClass.getResource(WORDNET_HOME))



  def allThingSynonyms() {
    io.Source.fromURL(this.getClass.getResource(THING_LIST_FILE)).getLines().foreach(thing => {
      println(thing)
      iterateSynonyms(thing)
      val words = thing.split("\\s+|_")
      if (words.length > 1)
        phraseSynonyms(words)
    })
  }

  def phraseSynonyms(words: Array[String]) {
    val wordSynonyms = new collection.mutable.MutableList[collection.mutable.MutableList[String]]()

    var phraseCount = 1;
    words.foreach(word => {
      var synonyms = getSynonyms(word)
      if (word.toLowerCase().equals("or")) {
        synonyms = new collection.mutable.MutableList[String]()
        synonyms += "or"
      }
      if (synonyms != null) {
        wordSynonyms += synonyms
        phraseCount *= synonyms.size
      }
    })

    val phrases = new collection.mutable.MutableList[String]()
    var i = 0
    while (i < phraseCount) {
      phrases ++= recursivePhraseBuilder(wordSynonyms, 0, "")
      i += 1
    }
    phrases.toSet[String].foreach(p => println("\t" + p))
  }

  def recursivePhraseBuilder(wordSynonyms: collection.mutable.MutableList[collection.mutable.MutableList[String]],
                             i: Int, phraseSoFar: String): collection.mutable.MutableList[String] = {
    val phrases = new collection.mutable.MutableList[String]()

    // base case
    if (i >= wordSynonyms.size) {
      phrases += phraseSoFar
    }
    else {

      wordSynonyms(i).foreach(w => {
        val newPhrase = phraseSoFar + " " + w
        phrases ++= recursivePhraseBuilder(wordSynonyms, i + 1, newPhrase)
      })
    }
    return phrases
  }

  def iterateSynonyms(thing: String) {
    val synonyms = getSynonyms(thing)
    if (synonyms != null) {
      synonyms.foreach(syn => {
        println("\t" + syn)
      })
    }
  }

  def getSynonyms(inputWord: String): collection.mutable.MutableList[String] = {
    dict.open()

    var pos = POS.NOUN
    if (inputWord.endsWith("ing"))
      pos = POS.VERB

    val lemma = PorterLemmatizer.lemmatize(inputWord)
    val idxWord = dict.getIndexWord(lemma, pos)
    val synonyms = collection.mutable.MutableList[String]()

    if (idxWord != null) {
      val wordID = idxWord.getWordIDs().get(0)
      // 1st meaning
      val word = dict.getWord(wordID)
      val words = word.getSynset.getWords
      // iterate over words associated with the synset

      var i = 0
      while (i < (words.size())) {
        synonyms += words.get(i).getLemma
        i += 1
      }

//      val hypernyms = synset.getRelatedSynsets(Pointer.HYPERNYM)

    }

    dict.close
    synonyms
  }

  def getHypernyms(inputWord : String): collection.mutable.MutableList[String] =
  {
    dict.open()
    // get the synset
    val idxWord = dict.getIndexWord(inputWord, POS.NOUN)
    val wordID = idxWord.getWordIDs().get(0) // 1st meaning
  val word = dict.getWord(wordID)
    val synset = word.getSynset()

    // get the hypernyms
    val hypernyms = synset.getRelatedSynsets(Pointer.HYPERNYM).toArray[ISynsetID](Array())

    // print out each hypernym’s id and synonyms
    var i = 0
    val results = collection.mutable.MutableList[String]()
    while (i < hypernyms.size)
    {
      val sid = hypernyms(i)
      i += 1
      val words = dict.getSynset(sid).getWords()
      var j = 0
      while ( j < words.size())
      {
        val w = words.get(j)
        j += 1
        results += w.getLemma()
      }
    }
  }

  def testDictionary() {
    // construct the dictionary object and open it
    val dict = new Dictionary(this.getClass.getResource(JWIWordNetWrap.WORDNET_HOME))
    dict.open()
    // look up first sense of the word "dog"
    val idxWord = dict.getIndexWord("dog", POS.NOUN)
    val wordID = idxWord.getWordIDs().get(0)
    val word = dict.getWord(wordID)
    println(" Id = " + wordID)
    println(" Lemma = " + word.getLemma())
    println(" Gloss = " + word.getSynset().getGloss())
  }


  def main(args: Array[String]) {
    val words = Seq("Labrador Retriever",
      "German Shepherd",
      "Beagle",
      "Golden Retriever",
      "Yorkshire Terrier",
      "Bulldog",
      "Boxer",
      "Poodle",
      "Dachshund",
      "Rottweiler",
      "Bulldog",
      "Golden Retriever",
      "Xoloitzcuintle",
      "Whippet")

    words.foreach(w => {
      val syn = JWIWordNetWrap.getHypernyms(w.toLowerCase())
      print(s"$w :")
      if (!syn.isEmpty)
        print(syn.mkString(":"))

      println()
    })
  }
}
