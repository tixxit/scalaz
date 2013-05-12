package scalaz

import std.option.{optionInstance, none, some}

/**
 * OptionT monad transformer.
 */
final case class OptionT[F[+_], +A](run: F[Option[A]]) extends TraverseTImpl[Option, OptionT, F, A] {
  self =>

  def isDefined(implicit F: Functor[F]): F[Boolean] = mapO(_.isDefined)

  def isEmpty(implicit F: Functor[F]): F[Boolean] = mapO(_.isEmpty)

  def fold[X](some: A => X, none: => X)(implicit F: Functor[F]): F[X] =
    mapO {
      case None => none
      case Some(a) => some(a)
    }

  def getOrElse[AA >: A](default: => AA)(implicit F: Functor[F]): F[AA] = mapO(_.getOrElse(default))

  def orElse[AA >: A](a: => OptionT[F, AA])(implicit F: Monad[F]): OptionT[F, AA] =
    OptionT(F.bind(run) {
      case None => a.run
      case x@Some(_) => F.point(x)
    })

  private def mapO[B](f: Option[A] => B)(implicit F: Functor[F]) = F.map(run)(f)
}

//
// Prioritized Implicits for type class instances
//

trait OptionTInstances extends TraverseTInstances[OptionT, Option] {
  implicit def optionTEqual[F[+_], A](implicit F0: Equal[F[Option[A]]]): Equal[OptionT[F, A]] = F0.contramap((_: OptionT[F, A]).run)
}

trait OptionTFunctions extends TraverseTFunctions[OptionT, Option] {
  def optionT[M[+_]] = traverseT[M]

  def monadTell[F[_, +_], W, A](implicit MT0: MonadTell[F, W]) = new OptionTMonadTell[F, W] {
    def MT = MT0
  }

  def monadListen[F[_, +_], W, A](implicit ML0: MonadListen[F, W]) = new OptionTMonadListen[F, W] {
    def MT = ML0
  }
}

object OptionT extends OptionTFunctions with OptionTInstances {
  implicit object T extends TraverseTWrapper[OptionT, Option] {
    def wrap[M[+_], A](run: M[Option[A]]): OptionT[M, A] = OptionT(run)
    def unwrap[M[+_], A](t: OptionT[M, A]): M[Option[A]] = t.run
  }
}

//
// Implementation traits for type class instances
//

private[scalaz] trait OptionTMonadTell[F[_, +_], W] extends MonadTell[({ type λ[α, β] = OptionT[({ type f[+x] = F[α, x] })#f, β] })#λ, W] with TraverseTMonad[OptionT, Option, ({ type λ[+α] = F[W, α] })#λ] with TraverseTHoist[OptionT, Option] {
  def MT: MonadTell[F, W]

  implicit def M = MT
  def N = Monad[Option]
  def TraverseN = Traverse[Option]
  def T = OptionT.T

  def writer[A](w: W, v: A): OptionT[({ type λ[+α] = F[W, α] })#λ, A] =
    liftM[({ type λ[+α] = F[W, α] })#λ, A](MT.writer(w, v))

  def some[A](v: => A): OptionT[({ type λ[+α] = F[W, α] })#λ, A] =
    OptionT.optionT[({ type λ[+α] = F[W, α] })#λ].apply[A](MT.point(Some(v)))

  def none[A]: OptionT[({ type λ[+α] = F[W, α] })#λ, A] =
    OptionT.optionT[({ type λ[+α] = F[W, α] })#λ].apply[A](MT.point(None))
}

private[scalaz] trait OptionTMonadListen[F[_, +_], W] extends MonadListen[({ type λ[α, β] = OptionT[({ type f[+x] = F[α, x] })#f, β] })#λ, W] with OptionTMonadTell[F, W] {
  def MT: MonadListen[F, W]

  def listen[A](ma: OptionT[({ type λ[+α] = F[W, α] })#λ, A]): OptionT[({ type λ[+α] = F[W, α] })#λ, (A, W)] = {
    val tmp = MT.bind[(Option[A], W), Option[(A, W)]](MT.listen(ma.run)) {
      case (None, _) => MT.point(None)
      case (Some(a), w) => MT.point(Some(a, w))
    }

    OptionT.optionT[({ type λ[+α] = F[W, α] })#λ].apply[(A, W)](tmp)
  }
}
