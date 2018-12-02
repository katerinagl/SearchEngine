package com.search

import org.scalatest.{BeforeAndAfterEach, WordSpec}

class SearchSpec extends WordSpec with BeforeAndAfterEach {
  override def beforeEach() {
    SimpleInMemorySearch.cleanAll
  }

  "Search engine " should {
    "find correct doc " in {
      val quickFox = SimpleInMemorySearch.addDocument("The quick brown fox jumps over the lazy dog")
      val twFox = SimpleInMemorySearch.addDocument("Twentieth Century Fox Film Corporation is an American film studio currently owned by Fox Entertainment Group.")
      val lorem = SimpleInMemorySearch.addDocument("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua")

      val res1 = SimpleInMemorySearch.search("brown fox jump")
      val res2 = SimpleInMemorySearch.search("fox film")
      assert(quickFox == res1.head.docId)
      assert(twFox == res2.head.docId)
    }

    "add documents to indexes correct " in {
      val simpleText = "one, two- three.Again"
      val simpleText2 = "two apples"
      SimpleInMemorySearch.addDocument(simpleText)
      SimpleInMemorySearch.addDocument(simpleText2)
      assert(SimpleInMemorySearch.index.size == 2)
      assert(SimpleInMemorySearch.index(0).tokens sameElements Array("one", "two", "three", "again"))
      assert(SimpleInMemorySearch.invertedIndex.size == 5)
      assert(SimpleInMemorySearch.invertedIndex("two") == Set(0, 1))
    }

    "get document by id " in {
      val text = "one, two- three.Again"
      val id = SimpleInMemorySearch.addDocument(text)
      assert(SimpleInMemorySearch.getDocument(id) == text)
    }
  }
}