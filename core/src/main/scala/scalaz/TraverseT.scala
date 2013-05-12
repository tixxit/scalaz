package scalaz

final case class TraverseT[N[+_], M[+_], +A](run: M[N[A]])
    extends TraverseTImpl[N, ({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ, M, A]

object TraverseT extends TraverseTInstances5 {
  implicit def instances[N[+_]] = new TraverseTInstances[({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ, N]
      with TraverseTWrapper[({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ, N] {
    def T = this
    def wrap[M[+_], A](mna: M[N[A]]): TraverseT[N, M, A] = TraverseT(mna)
    def unwrap[M[+_], A](t: TraverseT[N, M, A]): M[N[A]] = t.run
  }
}

trait TraverseTImpl[N[+_], T[M[+_], +A] <: TraverseTImpl[N, T, M, A], M[+_], +A] {
  val run: M[N[A]]

  def map[B](f: A => B)(implicit M: Functor[M], N: Functor[N], T: TraverseTWrapper[T, N]): T[M, B] =
    T.wrap(M.map(run)(N.map(_)(f)))

  def flatMap[B](f: A => T[M, B])(implicit M: Monad[M], N0: Monad[N],
      N1: Traverse[N], T: TraverseTWrapper[T, N]): T[M, B] =
    T.wrap(M.join(M.map(run) { na => M.map(N1.traverseImpl(na) { a => f(a).run })(N0.join) }))

  def foreach(f: A => Unit)(implicit M: Each[M], N: Each[N]): Unit =
    M.each(run) { na => N.each(na)(f) }

  def flatMapF[B](f: A => M[B])(implicit M: Monad[M], N: Traverse[N], T: TraverseTWrapper[T, N]): T[M, B] =
    T.wrap(M.join(M.map(run) { na => N.traverseImpl(na)(f) }))

  def ap[B](f: => T[M, A => B])(implicit M: Apply[M], N: Apply[N], T: TraverseTWrapper[T, N]): T[M, B] =
    T.wrap(M.ap(run)(M.map(f.run) { (nf: N[A => B]) =>
      { (na: N[A]) => N.ap(na)(nf) }
    }))

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit M: Foldable[M], N: Foldable[N]): Z =
    M.foldRight[N[A], Z](run, z) { (a, b) => N.foldRight[A, Z](a, b)(f) }

  def traverse[G[_], B](f: A => G[B])(implicit M: Traverse[M], N: Traverse[N],
      G: Applicative[G], T: TraverseTWrapper[T, N]): G[T[M, B]] =
    G.map(M.traverse(run) { na => N.traverse(na)(f) })(T.wrap(_))

  def exists(f: A => Boolean)(implicit M: Functor[M], N: Foldable[N]): M[Boolean] =
    M.map(run)(N.any(_)(f))

  def forall(f: A => Boolean)(implicit M: Functor[M], N: Foldable[N]): M[Boolean] =
    M.map(run)(N.all(_)(f))
}

trait TraverseTWrapper[T[M[_], A], N[_]] {
  def wrap[M[_], A](mna: M[N[A]]): T[M, A]
  def unwrap[M[_], A](t: T[M, A]): M[N[A]]
}

object TraverseTWrapper {
  @inline final def apply[T[M[_], A], N[_]](implicit T: TraverseTWrapper[T, N]) = T
}

trait TraverseTFunctor[T[M[_], A], N[_], M[_]] extends Functor[({ type λ[α] = T[M, α] })#λ] {
  implicit def T: TraverseTWrapper[T, N]
  implicit def M: Functor[M]
  implicit def N: Functor[N]

  override def map[A, B](mna: T[M, A])(f: A => B): T[M, B] =
    T.wrap(M.map(T.unwrap(mna))(N.map(_)(f)))
}

trait TraverseTApply[T[M[_], A], N[_], M[_]] extends TraverseTFunctor[T, N, M] with Apply[({ type λ[α] = T[M, α] })#λ] {
  implicit def M: Apply[M]
  implicit def N: Apply[N]

  def ap[A,B](ta: => T[M, A])(f: => T[M, A => B]): T[M, B] =
    T.wrap(M.ap(T.unwrap(ta))(M.map(T.unwrap(f)) { (nf: N[A => B]) =>
      { (na: N[A]) => N.ap(na)(nf) }
    }))
}

trait TraverseTApplicative[T[M[_], A], N[_], M[_]] extends TraverseTApply[T, N, M] with Applicative[({ type λ[α] = T[M, α] })#λ] {
  implicit def M: Applicative[M]
  implicit def N: Applicative[N]

  def point[A](a: => A): T[M, A] = T.wrap(M.point(N.point(a)))
}

trait TraverseTMonad[T[M[_], A], N[_], M[_]] extends TraverseTApplicative[T, N, M] with Monad[({ type λ[α] = T[M, α] })#λ] {
  implicit def M: Monad[M]
  implicit def N: Monad[N]
  implicit def TraverseN: Traverse[N]

  def bind[A, B](fa: T[M, A])(f: A => T[M, B]): T[M, B] =
    T.wrap(M.join(M.map(T.unwrap(fa)) { na =>
      M.map(TraverseN.traverseImpl(na) { a => T.unwrap(f(a)) })(N.join)
    }))
}

trait TraverseTFoldable[T[M[_], A], N[_], M[_]] extends Foldable.FromFoldr[({ type λ[α] = T[M, α] })#λ] {
  implicit def T: TraverseTWrapper[T, N]
  implicit def M: Foldable[M]
  implicit def N: Foldable[N]

  override def foldRight[A, B](ta: T[M, A], z: => B)(f: (A, => B) => B): B =
    M.foldRight[N[A], B](T.unwrap(ta), z) { (a, b) => N.foldRight[A, B](a, b)(f) }
}

trait TraverseTTraverse[T[M[_], A], N[_], M[_]] extends Traverse[({ type λ[α] = T[M, α] })#λ]
    with TraverseTFunctor[T, N, M] with TraverseTFoldable[T, N, M] {
  implicit def M: Traverse[M]
  implicit def N: Traverse[N]

  def traverseImpl[G[_]: Applicative, A, B](ta: T[M, A])(f: A => G[B]): G[T[M, B]] =
    Functor[G].map(M.traverse(T.unwrap(ta)) { na => N.traverse(na)(f) })(T.wrap(_))
}

trait TraverseTHoist[T[M[_], A], N[_]] extends Hoist[({ type λ[M[_], α] = T[M, α] })#λ] { self =>
  implicit def T: TraverseTWrapper[T, N]
  implicit def N: Monad[N]
  implicit def TraverseN: Traverse[N]

  def liftM[M[_], A](ma: M[A])(implicit M: Monad[M]): T[M, A] =
    T.wrap(M.map(ma) { a => N.point(a) })

  def hoist[M0[_]: Monad, M1[_]](f: M0 ~> M1) =
    new (({ type λ[α] = T[M0, α] })#λ ~> ({ type λ[α] = T[M1, α] })#λ) {
      def apply[A](ta: T[M0, A]): T[M1, A] = T.wrap(f.apply(T.unwrap(ta)))
    }

  implicit def apply[M[_]](implicit M0: Monad[M]): Monad[({type λ[α] = T[M, α]})#λ] =
    new TraverseTMonad[T, N, M] {
      val T = self.T
      val M = M0
      val N = self.N
      val TraverseN = self.TraverseN
    }
}

trait TraverseTMonadPlus[T[M[_], A], N[_], M[_]] extends MonadPlus[({ type λ[α] = T[M, α] })#λ]
    with TraverseTMonad[T, N, M] {
  implicit def M: Monad[M]
  implicit def N: MonadPlus[N]

  def empty[A]: T[M, A] = T.wrap(M point N.empty[A])
  def plus[A](a: T[M, A], b: => T[M, A]): T[M, A] =
    T.wrap(M.apply2(T.unwrap(a), T.unwrap(b)) { (na, nb) => N.plus[A](na, nb) })
}

trait TraverseTInstancesFunctor[T[M[_], A], N[_]] { self =>
  protected def T: TraverseTWrapper[T, N]

  implicit def traverseTFunctor[M[_]](implicit N0: Functor[N], M0: Functor[M]): Functor[({type λ[α] = T[M, α]})#λ] =
    new TraverseTFunctor[T, N, M] {
      def T = self.T
      def N = N0
      def M = M0
    }
}

trait TraverseTInstancesApply[T[M[_], A], N[_]] extends TraverseTInstancesFunctor[T, N] { self =>
  implicit def traverseTApply[M[_]](implicit N0: Apply[N], M0: Apply[M]): Apply[({type λ[α] = T[M, α]})#λ] =
    new TraverseTApply[T, N, M] {
      def T = self.T
      def N = N0
      def M = M0
    }
}

trait TraverseTInstancesApplicative[T[M[_], A], N[_]] extends TraverseTInstancesApply[T, N] { self =>
  implicit def traverseTApplicative[M[_]](implicit N0: Applicative[N], M0: Applicative[M]): Applicative[({type λ[α] = T[M, α]})#λ] =
    new TraverseTApplicative[T, N, M] {
      def T = self.T
      def N = N0
      def M = M0
    }
}

trait TraverseTInstancesMonad[T[M[_], A], N[_]] extends TraverseTInstancesApplicative[T, N] { self =>
  implicit def traverseTMonad[M[_]](implicit N0: Monad[N], N1: Traverse[N], M0: Monad[M]): Monad[({type λ[α] = T[M, α]})#λ] =
    new TraverseTMonad[T, N, M] {
      def T = self.T
      def N = N0
      def TraverseN = N1
      def M = M0
    }
}

trait TraverseTInstancesFoldable[T[M[_], A], N[_]] { self =>
  protected def T: TraverseTWrapper[T, N]

  implicit def traverseTFoldable[M[_]](implicit N0: Foldable[N], M0: Foldable[M]): Foldable[({type λ[α] = T[M, α]})#λ] =
    new TraverseTFoldable[T, N, M] {
      def T = self.T
      def N = N0
      def M = M0
    }
}

trait TraverseTInstancesTraverse[T[M[_], A], N[_]] extends TraverseTInstancesFoldable[T, N]
    with TraverseTInstancesFunctor[T, N] { self =>
  implicit def traverseTTraverse[M[_]](implicit N0: Traverse[N], M0: Traverse[M]): Traverse[({type λ[α] = T[M, α]})#λ] =
    new TraverseTTraverse[T, N, M] {
      def T = self.T
      def N = N0
      def M = M0
    }
}

trait TraverseTInstancesMonadPlus[T[M[_], A], N[_]] extends TraverseTInstancesMonad[T, N]
    with TraverseTInstancesTraverse[T, N] { self =>
  implicit def traverseTMonadPlus[M[_]](implicit N0: MonadPlus[N], N1: Traverse[N], M0: Monad[M]): MonadPlus[({type λ[α] = T[M, α]})#λ] =
    new TraverseTMonadPlus[T, N, M] {
      def T = self.T
      def N = N0
      def TraverseN = N1
      def M = M0
    }
}

trait TraverseTInstancesMonadTrans[T[M[_], A], N[_]] { self =>
  protected def T: TraverseTWrapper[T, N]

  implicit def traverseTMonadTrans(implicit N0: Monad[N], N1: Traverse[N]): Hoist[T] = new TraverseTHoist[T, N] {
    def T = self.T
    def N = N0
    def TraverseN = N1
  }
}

trait TraverseTFunctions[T[M[_], A], N[_]] {
  protected def T: TraverseTWrapper[T, N]

  def traverseT[M[_]] = new (({type λ[α] = M[N[α]]})#λ ~> ({type λ[α] = T[M, α]})#λ) {
    def apply[A](a: M[N[A]]): T[M, A] = T.wrap(a)
  }
}

trait TraverseTInstances[T[_[_], _], N[_]] extends TraverseTInstancesMonadTrans[T, N] with TraverseTInstancesMonadPlus[T, N]


private[scalaz] trait TraverseTInstances0 {
  implicit def traverseTFunctor[N[+_], M[+_]](implicit T0: TraverseTInstances[({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ, N],
      N0: Functor[N], M0: Functor[M]): Functor[({ type λ[+α] = TraverseT[N, M, α] })#λ] = T0.traverseTFunctor[M]

  implicit def traverseTFoldable[N[+_], M[+_]](implicit T0: TraverseTInstances[({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ, N],
      N0: Foldable[N], M0: Foldable[M]): Foldable[({ type λ[+α] = TraverseT[N, M, α] })#λ] = T0.traverseTFoldable[M]
}

private[scalaz] trait TraverseTInstances1 extends TraverseTInstances0 {
  implicit def traverseTApply[N[+_], M[+_]](implicit T0: TraverseTInstances[({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ, N],
      N0: Apply[N], M0: Apply[M]): Apply[({ type λ[+α] = TraverseT[N, M, α] })#λ] = T0.traverseTApply[M]
}

private[scalaz] trait TraverseTInstances2 extends TraverseTInstances1 {
  implicit def traverseTApplicatve[N[+_], M[+_]](implicit T0: TraverseTInstances[({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ, N],
      N0: Applicative[N], M0: Applicative[M]): Applicative[({ type λ[+α] = TraverseT[N, M, α] })#λ] = T0.traverseTApplicative[M]
}

private[scalaz] trait TraverseTInstances3 extends TraverseTInstances2 {
  implicit def traverseTMonad[N[+_], M[+_]](implicit T0: TraverseTInstances[({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ, N],
      N0: Monad[N], N1: Traverse[N], M0: Monad[M]): Monad[({ type λ[+α] = TraverseT[N, M, α] })#λ] = T0.traverseTMonad[M]
}

private[scalaz] trait TraverseTInstances4 extends TraverseTInstances3 {
  implicit def traverseTMonadPlus[N[+_], M[+_]](implicit T0: TraverseTInstances[({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ, N],
      N0: MonadPlus[N], N1: Traverse[N], M: Monad[M]): MonadPlus[({ type λ[+α] = TraverseT[N, M, α] })#λ] = T0.traverseTMonadPlus[M]
}

private[scalaz] trait TraverseTInstances5 extends TraverseTInstances4 {
  implicit def traverseTTraverse[N[+_], M[+_]](implicit T0: TraverseTInstances[({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ, N],
      N0: Traverse[N], M0: Traverse[M]): Traverse[({ type λ[+α] = TraverseT[N, M, α] })#λ] = T0.traverseTTraverse[M]

  implicit def traverseTMonadTrans[N[+_], M[+_]](implicit T0: TraverseTInstances[({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ, N],
      N0: MonadPlus[N], N1: Traverse[N]): MonadTrans[({ type λ[f[+_], +α] = TraverseT[N, f, α] })#λ] = T0.traverseTMonadTrans
}
