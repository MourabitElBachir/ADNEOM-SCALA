package un_sac_avec_des_items

import support.HandsOnSuite

class e3_on_a_besoin_de_la_covariance extends HandsOnSuite {

  /**
   *
   * La covariance est un mécanisme pour dire qu'un type paramêtrée varie de la même façon que son paramêtre.
   *
   * Si A <: B  (cela veut dire que A est une sous classe de B)
   * Et que Sac[A] est covariance sur A (Sac[+A])
   * Alors Sac[A] <: Sac[B]
   *
   * Ce mécanisme est nécessaire ici pour définir le Sac Vide.
   *
   * Quelque soit A dans les types, Nothing <: A
   * SacVide <: Sac[Nothing]
   * alors SacVide <: Sac[A]
   *
   *
   * @tparam A
   */
  sealed trait Sac[+A] {

    def items:Set[String]

    def map[B](fonction:A => B):Sac[B]

    def flatMap[B](fonction:A => Sac[B]):Sac[B]

    def filter(fonction:A => Boolean):Sac[A]

    def valeurOuSinon[B >: A](replacement:B):B

    def isEmpty:Boolean

    def addItems(items:Set[String]):Sac[A]

  }

  object Sac {
    def apply[A](valeur:A, items:Set[String] = Set.empty):Sac[A] = SacPlein(valeur, items)
  }

  case class SacVide(items:Set[String] = Set.empty) extends Sac[Nothing] {

    type A = Nothing

    override def map[B](fonction:A => B):Sac[B]  = SacVide(items)

    override def flatMap[B](fonction:A => Sac[B]):Sac[B]  = SacVide()

    override def filter(fonction:A => Boolean):Sac[A]  = SacVide(items)

    override def valeurOuSinon[B >: A](replacement:B):B = replacement

    override val isEmpty: Boolean = true

    def addItems(items:Set[String]):Sac[A] = this.copy(this.items.union(items))

  }

  case class SacPlein[A](valeur:A , items:Set[String] = Set.empty) extends Sac[A] {

    override def map[B](fonction:A => B):Sac[B]  = this.copy(fonction(valeur))

    override def flatMap[B](fonction:A => Sac[B]):Sac[B]  = {
      val res:Sac[B] = fonction(valeur)
      res.addItems(this.items)
    }

    override def filter(fonction:A => Boolean):Sac[A]  =
      fonction(valeur) match {
        case true => this.copy(valeur)
        case _ => SacVide()
      }

    override def valeurOuSinon[B >: A](replacement:B):B = valeur

    override val isEmpty: Boolean = false

    def addItems(items:Set[String]):Sac[A] = this.copy(valeur, this.items.union(items))

  }


  exercice("on peut ajouter des items au sac") {

    val s0 = Sac(0)

    s0.addItems(Set("un portable")).items should be(Set("un portable"))

    val v0 = SacVide()

    v0.addItems(Set("un portable")).items should be(Set("un portable"))

  }


  exercice("Un peu comme avant, l'application de fonction dans le conteneur") {
    val petitSacDeZero = Sac(0,Set("un portable"))

    petitSacDeZero.map(x => x + 1) match {
      case SacPlein(valeur, _ )  =>  valeur should be(1)

      case _ => fail("cela ne doit pas être un sac vide")
    }

  }




  exercice("La combinaison de Sac") {

    val petitSacDeZero = Sac(0,Set("un portable"))

    val grandSacDeA = Sac("A", Set("un pc"))

    val combinaison = for (p <- petitSacDeZero; g <- grandSacDeA) yield { p.toString + g}

    combinaison match {
      case SacPlein(valeur, items) => {
        valeur should be("0A")
        combinaison.items should be(Set("un portable", "un pc"))
      }
      case _ => fail("cela ne doit pas être un sac vide")
    }
  }

  exercice("Le filtrage") {
    val petitSacDeZero = Sac(0,Set("un portable"))

    assert(petitSacDeZero.filter(x => x > 1).isEmpty)
  }



}
