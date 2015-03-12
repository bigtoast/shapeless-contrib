package shapeless.contrib.spire

import shapeless.contrib.scalacheck._

import spire.algebra._
import spire.std.int._
import spire.std.long._
import spire.laws._

import org.typelevel.discipline.scalatest.Discipline

import org.scalatest.FunSuite

import org.scalacheck.Arbitrary

class ProductTest extends FunSuite with Discipline {

  case class OneElem(n: Int)
  //import shapeless._
  //import LabelledGeneric._
  //import EqCompanion._
  //import ArbitraryCompanion._
  //import Lazy._
  //implicit val oneG = Generic[OneElem]
  //implicit val lazyOne =
  //implicit val gen = implicitly[Generic[OneElem]]
  //implicit val lz = Lazy.mkLazy[Eq[gen.Repr]]
  //implicitly[Lazy[Eq[gen.Repr]]]
  //implicitly[Lazy[Int]]
  //implicitly[Eq[Int]]
  //implicitly[Eq[OneElem]]
  //import ArbitraryCompanion._
  //implicitly[Arbitrary[Int]]
  //implicitly[Arbitrary[OneElem]]
  checkAll("one element", GroupLaws[OneElem].additiveAbGroup)
  checkAll("one element", RingLaws[OneElem].multiplicativeSemigroup)

  case class TwoElem(n: Int, m: Long)

  checkAll("two elements", GroupLaws[TwoElem].additiveAbGroup)
  checkAll("two elements", RingLaws[OneElem].multiplicativeSemigroup)

}
