package com.search

case class SearchResult(docId: Int, score: Double)

case class SavedDocument(fullText: String, tokens: Array[String])

object SimpleInMemorySearch extends SearchEngine {
  private[search] val index = scala.collection.mutable.Map[Int, SavedDocument]()
  private[search] val invertedIndex = scala.collection.mutable.Map[String, Set[Int]]()
  private var currentIndex = 0
  private val punctuationList = List(',', '.', '-', ':', ';', ''', '"')

  def addDocument(doc: String): Int = {
    val allTokens = tokenizeDocument(doc)
    val docId = addToIndex(doc, allTokens)
    addToInverted(allTokens, docId)
    docId
  }

  def getDocument(docId: Int): String = {
    index(docId).fullText
  }

  def search(query: String): Array[SearchResult] = {
    val tokenizedQuery = tokenizeDocument(query)
    val tfIdfQuery = calculateTfIdf(tokenizedQuery)
    val docForEachTerm = tokenizedQuery.flatMap(t => invertedIndex.get(t)).flatten.distinct

    val allSimilarity = docForEachTerm.map { id =>
      val allTokensInDoc = index(id).tokens
      val tfIdfToTokenMap = allTokensInDoc.zip(calculateTfIdf(allTokensInDoc)).toMap
      val tfIdfDoc = tokenizedQuery.map(q => tfIdfToTokenMap.getOrElse(q, 0.0))
      val similarity = cosineSimilarity(tfIdfQuery, tfIdfDoc)
      SearchResult(id, similarity)
    }

    allSimilarity.sortBy(-_.score)
  }

  private[search] def cleanAll = {
    index.clear()
    invertedIndex.clear()
    currentIndex = 0
  }

  private def tokenizeDocument(doc: String): Array[String] = {
    cleanUpPunctuation(doc).split("\\s+").map(_.trim.toLowerCase)
  }

  private def cleanUpPunctuation(doc: String) = {
    doc.foldLeft[String]("") { (all, char) =>
      val replaced = if (punctuationList.contains(char)) ' ' else char
      all + replaced
    }
  }

  private def addToInverted(allTokens: Array[String], docId: Int) = {
    allTokens.foreach { token =>
      val docList = invertedIndex.get(token)
      invertedIndex.update(token, docList.map(s => s ++ Set(docId)).getOrElse(Set(docId)))
    }
  }

  private def addToIndex(doc: String, allTokens: Array[String]) = {
    val idx = currentIndex
    index += (idx -> SavedDocument(doc, allTokens))
    currentIndex += 1
    idx
  }

  private def calculateTfIdf(doc: Array[String]) = {
    val allDocs = currentIndex
    val size = doc.length
    val tf = doc.groupBy(key => key).map(tuple => tuple._2.length.toDouble / size)
    val docForEachTerm = doc.map(invertedIndex.get(_).map(_.size))
    val idf: Array[Double] = docForEachTerm.map(_.map(docsWithTerm => Math.log(allDocs.toDouble / docsWithTerm)).getOrElse(0.0))
    tf.zip(idf).map(t => t._1 * t._2).toArray
  }

  private def cosineSimilarity(tfIdfQuery: Array[Double], tfIdfDocument: Array[Double]) = {
    val dotProduct = tfIdfDocument.zip(tfIdfQuery).map(t => t._1 * t._2).sum
    val docSumDocument = docSum(tfIdfDocument)
    val docSumQuery = docSum(tfIdfQuery)
    dotProduct / (docSumDocument * docSumQuery)
  }

  private def docSum(array: Array[Double]) = {
    Math.pow(array.map(n => Math.pow(n, 2)).sum, 1 / 2)
  }
}
