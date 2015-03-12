package shapeless.contrib

import shapeless._

import org.scalacheck.{Gen, Arbitrary, Shrink}

package object scalacheck {

  // TODO this is terrible
  private lazy val _emptyCoproduct: Gen[Nothing] = Gen.fail

  implicit object ArbitraryCompanion extends TypeClassCompanion[Arbitrary] {
    val typeClass :TypeClass[Arbitrary] = new TypeClass[Arbitrary] {

      def emptyProduct = Arbitrary(Gen.const(HNil))

      def product[H, T <: HList](h: Arbitrary[H], t: Arbitrary[T]) =
        Arbitrary(Gen.sized { size =>
          if (size == 0)
            Gen.fail
          else {
            val resizedH = Gen.resize(size.abs/2, h.arbitrary)
            val resizedT = Gen.resize(size.abs - size.abs/2, t.arbitrary)
            for { h <- resizedH; t <- resizedT }
            yield h :: t
          }})

      def coproduct[L, R <: Coproduct](l: => Arbitrary[L], r: => Arbitrary[R]) = {
        val rGen = r.arbitrary
        val gens: List[Gen[L :+: R]] =
          l.arbitrary.map(Inl(_): L :+: R) ::
        (if (rGen == _emptyCoproduct) Nil else List(rGen.map(Inr(_): L :+: R)))
        Arbitrary(Gen.oneOf(gens).flatMap(identity))
      }

      def emptyCoproduct =
        Arbitrary(_emptyCoproduct)

      def project[A, B](b: => Arbitrary[B], ab: A => B, ba: B => A) =
        Arbitrary(b.arbitrary.map(ba))

    }
  }

  implicit object ShrinkCompanion extends TypeClassCompanion[Shrink] {
    val typeClass :TypeClass[Shrink] = new TypeClass[Shrink] {

      def emptyProduct = Shrink(_ => Stream.empty)

      def product[F, T <: HList](f: Shrink[F], t: Shrink[T]) = Shrink { case a :: b ⇒
        f.shrink(a).map( _ :: b) append
        t.shrink(b).map(a :: _)
      }

      def project[A, B](b: => Shrink[B], ab: A => B, ba: B => A) = Shrink { a =>
        b.shrink(ab(a)).map(ba)
      }

      def coproduct[L, R <: Coproduct](sl: => Shrink[L], sr: => Shrink[R]) = Shrink { lr =>
        lr match {
          case Inl(l) ⇒ sl.shrink(l).map(Inl.apply)
          case Inr(r) ⇒ sr.shrink(r).map(Inr.apply)
        }
      }

      def emptyCoproduct: Shrink[CNil] = Shrink(_ => Stream.empty)

    }
  }


  implicit def deriveArbitrary[T,F](
    implicit gen :Generic.Aux[T,F], lzy :Lazy[Arbitrary[F]] ) :Arbitrary[T] = ArbitraryCompanion.deriveInstance[T,F]
/*
  implicit def deriveShrink[T,F](
    implicit gen :Generic.Aux[T,F], lzy :Lazy[Shrink[F]] ) :Shrink[T] = ShrinkCompanion.deriveInstance[T,F]

  implicit def deriveArbitrary[T,F](
    implicit ev :TypeClassCompanion[Arbitrary],
    gen :Generic.Aux[T,F], lzy :Lazy[Arbitrary[F]] ) :Arbitrary[T] = ev.deriveInstance[T,F]

  implicit def deriveShrink[T,F](
    implicit ev :TypeClassCompanion[Shrink],
    gen :Generic.Aux[T,F], lzy :Lazy[Shrink[F]] ) :Shrink[T] = ev.deriveInstance[T,F]
 */
}
