package pas_suivant

import support.HandsOnSuite

/**
*   Un Set est une collection qui ne contient que des éléments distincts, comme en Java.
*
*   Quelques liens :
*     - http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Set
*     - http://docs.scala-lang.org/overviews/collections/sets.html
*/

class e6_sets extends HandsOnSuite {

  /**
  * Création d'un Set
  */
  exercice("Création d'un Set") {
    val mySet = Set("Sud", "Est", "Ouest", "Nord")
    mySet.size should be(4)

    val myOtherSet = Set("Sud", "Est", "Sud", "Nord")
    myOtherSet.size should be(3)
  }

  /**
  * Quelques opérations : les fonctions +, -, --  et contains
  */
  exercice("Opérations sur les Sets") {
    // addition
    val mySet = Set("Sud", "Est", "Sud")
    val aNewSet = mySet + "Nord"

    aNewSet.contains("Nord") should be(true)

    // suppression
    val mySetBis = Set("Sud", "Est", "Ouest", "Nord")
    val aNewSetBis = mySetBis - "Nord"
    // la méthode contains
    aNewSetBis.contains("Nord") should be(false)

    // suppressions multiples
    val myOtherSet = Set("Sud", "Est", "Ouest", "Nord")
    val aNewOtherSet = myOtherSet -- List("Ouest", "Nord")

    aNewOtherSet.contains("Nord") should be(false)
    aNewOtherSet.contains("Ouest") should be(false)
  }
}
