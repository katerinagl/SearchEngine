package com.search

trait SearchEngine {
  def addDocument(doc: String): Int

  def getDocument(docId: Int): String

  def search(query: String): Array[SearchResult]
}
