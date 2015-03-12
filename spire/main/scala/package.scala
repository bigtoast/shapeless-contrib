package shapeless.contrib

import _root_.spire.math._
import _root_.spire.algebra._

import shapeless._

package object spire {

  // Instances

  implicit object EqCompanion extends ProductTypeClassCompanion[Eq] {
    //implicit def EqI: ProductTypeClass[Eq] = new ProductTypeClass[Eq] with Empty {
    val typeClass : ProductTypeClass[Eq] = new ProductTypeClass[Eq] with Empty {
      def product[F, T <: HList](f: Eq[F], t: Eq[T]) =
        new ProductEq[F, T] { def F = f; def T = t }
      def project[A, B](b: => Eq[B], ab: A => B, ba: B => A) =
        b on ab

      def coproduct[L,R <: Coproduct](l : => Eq[L], r : => Eq[R] ) =
        new SumEq[L,R] { def L = l; def R = r }

      def emptyCoproduct = ???
    }
  }

  implicit object OrderCompanion extends ProductTypeClassCompanion[Order] {
    val typeClass : ProductTypeClass[Order] = new ProductTypeClass[Order] with Empty {
      def product[F, T <: HList](f: Order[F], t: Order[T]) =
        new ProductOrder[F, T] { def F = f; def T = t }
      def project[A, B](b: => Order[B], ab: A => B, ba: B => A) =
        b on ab
    }
  }

  implicit object SemigroupCompanion extends ProductTypeClassCompanion[Semigroup] {
    val typeClass: ProductTypeClass[Semigroup] = new ProductTypeClass[Semigroup] with Empty {
      def product[F, T <: HList](f: Semigroup[F], t: Semigroup[T]) =
        new ProductSemigroup[F, T] { def F = f; def T = t }
      def project[A, B](b: => Semigroup[B], ab: A => B, ba: B => A) =
        new IsomorphicSemigroup[A, B] { def B = b; def to = ab; def from = ba }
    }
  }

  implicit object MonoidCompanion extends ProductTypeClassCompanion[Monoid] {
    val typeClass : ProductTypeClass[Monoid] = new ProductTypeClass[Monoid] with Empty {
      def product[F, T <: HList](f: Monoid[F], t: Monoid[T]) =
        new ProductMonoid[F, T] { def F = f; def T = t }
      def project[A, B](b: => Monoid[B], ab: A => B, ba: B => A) =
        new IsomorphicMonoid[A, B] { def B = b; def to = ab; def from = ba }
    }
  }

  implicit object GroupCompanion extends ProductTypeClassCompanion[Group] {
    val typeClass : ProductTypeClass[Group] = new ProductTypeClass[Group] with Empty {
      def product[F, T <: HList](f: Group[F], t: Group[T]) =
        new ProductGroup[F, T] { def F = f; def T = t }
      def project[A, B](b: => Group[B], ab: A => B, ba: B => A) =
        new IsomorphicGroup[A, B] { def B = b; def to = ab; def from = ba }
    }
  }

  implicit object AbGroupCompanion extends ProductTypeClassCompanion[AbGroup] {
    val typeClass : ProductTypeClass[AbGroup] = new ProductTypeClass[AbGroup] with Empty {
      def product[F, T <: HList](f: AbGroup[F], t: AbGroup[T]) =
        new ProductAbGroup[F, T] { def F = f; def T = t }
      def project[A, B](b: => AbGroup[B], ab: A => B, ba: B => A) =
        new IsomorphicAbGroup[A, B] { def B = b; def to = ab; def from = ba }
    }
  }

  implicit object AdditiveSemigroupCompanion extends ProductTypeClassCompanion[AdditiveSemigroup] {
    val typeClass : ProductTypeClass[AdditiveSemigroup] = new ProductTypeClass[AdditiveSemigroup] with Empty {
      def product[F, T <: HList](f: AdditiveSemigroup[F], t: AdditiveSemigroup[T]) =
        new ProductAdditiveSemigroup[F, T] { def F = f; def T = t }
      def project[A, B](b: => AdditiveSemigroup[B], ab: A => B, ba: B => A) =
        new IsomorphicAdditiveSemigroup[A, B] { def B = b; def to = ab; def from = ba }
    }
  }

  implicit object AdditiveMonoidCompanion extends ProductTypeClassCompanion[AdditiveMonoid] {
    val typeClass : ProductTypeClass[AdditiveMonoid] = new ProductTypeClass[AdditiveMonoid] with Empty {
      def product[F, T <: HList](f: AdditiveMonoid[F], t: AdditiveMonoid[T]) =
        new ProductAdditiveMonoid[F, T] { def F = f; def T = t }
      def project[A, B](b: => AdditiveMonoid[B], ab: A => B, ba: B => A) =
        new IsomorphicAdditiveMonoid[A, B] { def B = b; def to = ab; def from = ba }
    }
  }

  implicit object AdditiveGroupCompanion extends ProductTypeClassCompanion[AdditiveGroup] {
    val typeClass : ProductTypeClass[AdditiveGroup] = new ProductTypeClass[AdditiveGroup] with Empty {
      def product[F, T <: HList](f: AdditiveGroup[F], t: AdditiveGroup[T]) =
        new ProductAdditiveGroup[F, T] { def F = f; def T = t }
      def project[A, B](b: => AdditiveGroup[B], ab: A => B, ba: B => A) =
        new IsomorphicAdditiveGroup[A, B] { def B = b; def to = ab; def from = ba }
    }
  }

  implicit object AdditiveAbGroupCompanion extends ProductTypeClassCompanion[AdditiveAbGroup] {
    val typeClass : ProductTypeClass[AdditiveAbGroup] = new ProductTypeClass[AdditiveAbGroup] with Empty {
      def product[F, T <: HList](f: AdditiveAbGroup[F], t: AdditiveAbGroup[T]) =
        new ProductAdditiveAbGroup[F, T] { def F = f; def T = t }
      def project[A, B](b: => AdditiveAbGroup[B], ab: A => B, ba: B => A) =
        new IsomorphicAdditiveAbGroup[A, B] { def B = b; def to = ab; def from = ba }
    }
  }

  implicit object MultiplicativeSemigroupCompanion extends ProductTypeClassCompanion[MultiplicativeSemigroup] {
    val typeClass : ProductTypeClass[MultiplicativeSemigroup] = new ProductTypeClass[MultiplicativeSemigroup] with Empty {
      def product[F, T <: HList](f: MultiplicativeSemigroup[F], t: MultiplicativeSemigroup[T]) =
        new ProductMultiplicativeSemigroup[F, T] { def F = f; def T = t }
      def project[A, B](b: => MultiplicativeSemigroup[B], ab: A => B, ba: B => A) =
        new IsomorphicMultiplicativeSemigroup[A, B] { def B = b; def to = ab; def from = ba }
    }
  }

  implicit object MultiplicativeMonoidCompanion extends ProductTypeClassCompanion[MultiplicativeMonoid] {
    val typeClass : ProductTypeClass[MultiplicativeMonoid] = new ProductTypeClass[MultiplicativeMonoid] with Empty {
      def product[F, T <: HList](f: MultiplicativeMonoid[F], t: MultiplicativeMonoid[T]) =
        new ProductMultiplicativeMonoid[F, T] { def F = f; def T = t }
      def project[A, B](b: => MultiplicativeMonoid[B], ab: A => B, ba: B => A) =
        new IsomorphicMultiplicativeMonoid[A, B] { def B = b; def to = ab; def from = ba }
    }
  }

  implicit object MultiplicativeGroupCompanion extends ProductTypeClassCompanion[MultiplicativeGroup] {
    val typeClass : ProductTypeClass[MultiplicativeGroup] = new ProductTypeClass[MultiplicativeGroup] with Empty {
      def product[F, T <: HList](f: MultiplicativeGroup[F], t: MultiplicativeGroup[T]) =
        new ProductMultiplicativeGroup[F, T] { def F = f; def T = t }
      def project[A, B](b: => MultiplicativeGroup[B], ab: A => B, ba: B => A) =
        new IsomorphicMultiplicativeGroup[A, B] { def B = b; def to = ab; def from = ba }
    }
  }

  implicit object MultiplicativeAbGroupCompanion extends ProductTypeClassCompanion[MultiplicativeAbGroup] {
    val typeClass: ProductTypeClass[MultiplicativeAbGroup] = new ProductTypeClass[MultiplicativeAbGroup] with Empty {
      def product[F, T <: HList](f: MultiplicativeAbGroup[F], t: MultiplicativeAbGroup[T]) =
        new ProductMultiplicativeAbGroup[F, T] { def F = f; def T = t }
      def project[A, B](b: => MultiplicativeAbGroup[B], ab: A => B, ba: B => A) =
        new IsomorphicMultiplicativeAbGroup[A, B] { def B = b; def to = ab; def from = ba }
    }
  }


  // Boilerplate

  implicit def deriveEq[T,G](
    implicit ev: ProductTypeClassCompanion[Eq],
    gen:Generic.Aux[T,G], lz :Lazy[Eq[G]] ): Eq[T] = ev.deriveInstance[T,G]

  implicit def deriveOrder[T,G](
    implicit ev: ProductTypeClassCompanion[Order],
    gen:Generic.Aux[T,G], lz :Lazy[Order[G]] ): Order[T] = ev.deriveInstance[T,G]

  implicit def deriveSemigroup[T,G](
    implicit ev: ProductTypeClassCompanion[Semigroup],
    gen:Generic.Aux[T,G], lz :Lazy[Semigroup[G]] ): Semigroup[T] = ev.deriveInstance[T,G]

  implicit def deriveMonoid[T,G](
    implicit ev: ProductTypeClassCompanion[Monoid],
    gen:Generic.Aux[T,G], lz :Lazy[Monoid[G]] ): Monoid[T] = ev.deriveInstance[T,G]

  implicit def deriveGroup[T,G](
    implicit ev: ProductTypeClassCompanion[Group],
    gen:Generic.Aux[T,G], lz :Lazy[Group[G]] ): Group[T] = ev.deriveInstance[T,G]

  implicit def deriveAbGroup[T,G](
    implicit ev: ProductTypeClassCompanion[AbGroup],
    gen:Generic.Aux[T,G], lz :Lazy[AbGroup[G]] ): AbGroup[T] = ev.deriveInstance[T,G]

  implicit def deriveAdditiveSemigroup[T,G](
    implicit ev: ProductTypeClassCompanion[AdditiveSemigroup],
    gen:Generic.Aux[T,G], lz :Lazy[AdditiveSemigroup[G]] ): AdditiveSemigroup[T] = ev.deriveInstance[T,G]

  implicit def deriveAdditiveMonoid[T,G](
    implicit ev: ProductTypeClassCompanion[AdditiveMonoid],
    gen:Generic.Aux[T,G], lz :Lazy[AdditiveMonoid[G]] ): AdditiveMonoid[T] = ev.deriveInstance[T,G]

  implicit def deriveAdditiveGroup[T,G](
    implicit ev: ProductTypeClassCompanion[AdditiveGroup],
    gen:Generic.Aux[T,G], lz :Lazy[AdditiveGroup[G]] ): AdditiveGroup[T] = ev.deriveInstance[T,G]

  implicit def deriveAdditiveAbGroup[T,G](
    implicit ev: ProductTypeClassCompanion[AdditiveAbGroup],
    gen:Generic.Aux[T,G], lz :Lazy[AdditiveAbGroup[G]] ): AdditiveAbGroup[T] = ev.deriveInstance[T,G]

  implicit def deriveMultiplicativeSemigroup[T,G](
    implicit ev: ProductTypeClassCompanion[MultiplicativeSemigroup],
    gen:Generic.Aux[T,G], lz :Lazy[MultiplicativeSemigroup[G]] ): MultiplicativeSemigroup[T] = ev.deriveInstance[T,G]

  implicit def deriveMultiplicativeMonoid[T,G](
    implicit ev: ProductTypeClassCompanion[MultiplicativeMonoid],
    gen:Generic.Aux[T,G], lz :Lazy[MultiplicativeMonoid[G]] ): MultiplicativeMonoid[T] = ev.deriveInstance[T,G]

  implicit def deriveMultiplicativeGroup[T,G](
    implicit ev: ProductTypeClassCompanion[MultiplicativeGroup],
    gen:Generic.Aux[T,G], lz :Lazy[MultiplicativeGroup[G]] ): MultiplicativeGroup[T] = ev.deriveInstance[T,G]

  implicit def deriveMultiplicativeAbGroup[T,G](
    implicit ev: ProductTypeClassCompanion[MultiplicativeAbGroup],
    gen:Generic.Aux[T,G], lz :Lazy[MultiplicativeAbGroup[G]] ): MultiplicativeAbGroup[T] = ev.deriveInstance[T,G]

}
