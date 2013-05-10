package scalaz

trait TraverseT[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A], M[+_], +A] {
  val run: M[N[A]]

  def map[B](f: A => B)(implicit M: Functor[M], N: Functor[N], T: TraverseTWrapper[N, T]): T[M, B] =
    T.wrap(M.map(run)(N.map(_)(f)))

  def flatMap[B](f: A => T[M, B])(implicit M: Monad[M], N0: Monad[N],
      N1: Traverse[N], T: TraverseTWrapper[N, T]): T[M, B] =
    T.wrap(M.join(M.map(run) { na => M.map(N1.traverseImpl(na) { a => f(a).run })(N0.join) }))

  def foreach(f: A => Unit)(implicit M: Each[M], N: Each[N]): Unit =
    M.each(run) { na => N.each(na)(f) }

  def flatMapF[B](f: A => M[B])(implicit M: Monad[M], N: Traverse[N], T: TraverseTWrapper[N, T]): T[M, B] =
    T.wrap(M.join(M.map(run) { na => N.traverseImpl(na)(f) }))

  def ap[B](f: => T[M, A => B])(implicit M: Apply[M], N: Apply[N], T: TraverseTWrapper[N, T]): T[M, B] =
    T.wrap(M.ap(run)(M.map(f.run) { (nf: N[A => B]) =>
      { (na: N[A]) => N.ap(na)(nf) }
    }))

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit M: Foldable[M], N: Foldable[N]): Z =
    M.foldRight[N[A], Z](run, z) { (a, b) => N.foldRight[A, Z](a, b)(f) }

  def traverse[G[_], B](f: A => G[B])(implicit M: Traverse[M], N: Traverse[N],
      G: Applicative[G], T: TraverseTWrapper[N, T]): G[T[M, B]] =
    G.map(M.traverse(run) { na => N.traverse(na)(f) })(T.wrap(_))
}

private[scalaz] trait TraverseTWrapper[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]] {
  def wrap[M[+_], A](mna: M[N[A]]): T[M, A]
}

private[scalaz] trait TraverseTFunctor[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A], M[+_]]
    extends Functor[({ type λ[α] = T[M, α] })#λ] {
  implicit def T: TraverseTWrapper[N, T]
  implicit def M: Functor[M]
  implicit def N: Functor[N]

  override def map[A, B](mna: T[M, A])(f: A => B): T[M, B] = mna map f
}

private[scalaz] trait TraverseTApply[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A], M[+_]]
    extends TraverseTFunctor[N, T, M] with Apply[({ type λ[α] = T[M, α] })#λ] {
  implicit def M: Apply[M]
  implicit def N: Apply[N]

  def ap[A,B](ta: => T[M, A])(f: => T[M, A => B]): T[M, B] =
    T.wrap(M.ap(ta.run)(M.map(f.run) { (nf: N[A => B]) =>
      { (na: N[A]) => N.ap(na)(nf) }
    }))
}

private[scalaz] trait TraverseTApplicative[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A], M[+_]]
    extends TraverseTApply[N, T, M] with Applicative[({ type λ[α] = T[M, α] })#λ] {
  implicit def M: Applicative[M]
  implicit def N: Applicative[N]

  def point[A](a: => A): T[M, A] = T.wrap(M.point(N.point(a)))
}

private[scalaz] trait TraverseTMonad[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A], M[+_]]
    extends TraverseTApplicative[N, T, M] with Monad[({ type λ[α] = T[M, α] })#λ] {
  implicit def M: Monad[M]
  implicit def N: Monad[N]
  implicit def TraverseN: Traverse[N]

  def bind[A, B](fa: T[M, A])(f: A => T[M, B]): T[M, B] =
    fa flatMap f
}

private[scalaz] trait TraverseTFoldable[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A], M[+_]]
    extends Foldable.FromFoldr[({ type λ[α] = T[M, α] })#λ] {
  implicit def T: TraverseTWrapper[N, T]
  implicit def M: Foldable[M]
  implicit def N: Foldable[N]

  override def foldRight[A, B](ta: T[M, A], z: => B)(f: (A, => B) => B): B =
    ta.foldRight(z)(f)
}

private[scalaz] trait TraverseTTraverse[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A], M[+_]]
    extends Traverse[({ type λ[α] = T[M, α] })#λ] with TraverseTFunctor[N, T, M] with TraverseTFoldable[N, T, M] {
  implicit def M: Traverse[M]
  implicit def N: Traverse[N]

  def traverseImpl[G[_]: Applicative, A, B](ta: T[M, A])(f: A => G[B]): G[T[M, B]] =
    ta traverse f
}

private[scalaz] trait TraverseTHoist[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]]
    extends Hoist[({ type λ[M[+_], α] = T[M, α] })#λ] { self =>
  implicit def T: TraverseTWrapper[N, T]
  implicit def N: Monad[N]
  implicit def TraverseN: Traverse[N]

  def liftM[M[+_], A](ma: M[A])(implicit M: Monad[M]): T[M, A] =
    T.wrap(M.map(ma) { a => N.point(a) })

  def hoist[M0[+_]: Monad, M1[+_]](f: M0 ~> M1) =
    new (({ type λ[α] = T[M0, α] })#λ ~> ({ type λ[α] = T[M1, α] })#λ) {
      def apply[A](ta: T[M0, A]): T[M1, A] = T.wrap(f.apply(ta.run))
    }

  implicit def apply[M[+_]](implicit M0: Monad[M]): Monad[({type λ[α] = T[M, α]})#λ] =
    new TraverseTMonad[N, T, M] {
      val T = self.T
      val M = M0
      val N = self.N
      val TraverseN = self.TraverseN
    }
}

private[scalaz] trait TraverseTMonadPlus[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A], M[+_]]
    extends MonadPlus[({ type λ[α] = T[M, α] })#λ] with TraverseTMonad[N, T, M] {
  implicit def M: Monad[M]
  implicit def N: MonadPlus[N]

  def empty[A]: T[M, A] = T.wrap(M point N.empty[A])
  def plus[A](a: T[M, A], b: => T[M, A]): T[M, A] =
    T.wrap(M.apply2(a.run, b.run) { (na, nb) => N.plus[A](na, nb) })
}

trait TraverseTInstancesFunctor[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]] { self =>
  implicit def T: TraverseTWrapper[N, T]
  implicit def N: Functor[N]

  implicit def traverseTFunctor[M[+_]](implicit M0: Functor[M]): Functor[({type λ[α] = T[M, α]})#λ] =
    new TraverseTFunctor[N, T, M] {
      def T = self.T
      def N = self.N
      def M = M0
    }
}

trait TraverseTInstancesApply[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]]
    extends TraverseTInstancesFunctor[N, T] { self =>
  implicit def N: Apply[N]

  implicit def traverseTApply[M[+_]](implicit M0: Apply[M]): Apply[({type λ[α] = T[M, α]})#λ] =
    new TraverseTApply[N, T, M] {
      def T = self.T
      def N = self.N
      def M = M0
    }
}

trait TraverseTInstancesApplicative[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]]
    extends TraverseTInstancesApply[N, T] { self =>
  implicit def N: Applicative[N]

  implicit def traverseTApplicative[M[+_]](implicit M0: Applicative[M]): Applicative[({type λ[α] = T[M, α]})#λ] =
    new TraverseTApplicative[N, T, M] {
      def T = self.T
      def N = self.N
      def M = M0
    }
}

trait TraverseTInstancesMonad[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]]
    extends TraverseTInstancesApplicative[N, T] { self =>
  implicit def N: Monad[N]
  implicit def TraverseN: Traverse[N]

  implicit def traverseTMonad[M[+_]](implicit M0: Monad[M]): Monad[({type λ[α] = T[M, α]})#λ] =
    new TraverseTMonad[N, T, M] {
      def T = self.T
      def N = self.N
      def TraverseN = self.TraverseN
      def M = M0
    }
}

trait TraverseTInstancesFoldable[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]] { self =>
  implicit def T: TraverseTWrapper[N, T]
  implicit def TraverseN: Foldable[N]

  implicit def traverseTFoldable[M[+_]](implicit M0: Foldable[M]): Foldable[({type λ[α] = T[M, α]})#λ] =
    new TraverseTFoldable[N, T, M] {
      def T = self.T
      def N = self.TraverseN
      def M = M0
    }
}

trait TraverseTInstancesTraverse[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]]
    extends TraverseTInstancesFoldable[N, T] with TraverseTInstancesFunctor[N, T] { self =>
  implicit def TraverseN: Traverse[N]

  implicit def traverseTTraverse[M[+_]](implicit M0: Traverse[M]): Traverse[({type λ[α] = T[M, α]})#λ] =
    new TraverseTTraverse[N, T, M] {
      def T = self.T
      def N = self.TraverseN
      def M = M0
    }
}

trait TraverseTInstancesMonadPlus[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]]
    extends TraverseTInstancesMonad[N, T] with TraverseTInstancesTraverse[N, T] { self =>
  implicit def N: MonadPlus[N]
  implicit def TraverseN: Traverse[N]

  implicit def traverseTMonadPlus[M[+_]](implicit M0: Monad[M]): MonadPlus[({type λ[α] = T[M, α]})#λ] =
    new TraverseTMonadPlus[N, T, M] {
      def T = self.T
      def N = self.N
      def TraverseN = self.TraverseN
      def M = M0
    }
}

trait TraverseTInstancesMonadTrans[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]] { self =>
  implicit def T: TraverseTWrapper[N, T]
  implicit def N: Monad[N]
  implicit def TraverseN: Traverse[N]

  implicit def optionTMonadTrans: Hoist[T] = new TraverseTHoist[N, T] {
    def T = self.T
    def N = self.N
    def TraverseN = self.TraverseN
  }
}

trait TraverseTFunctions[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]] {
  implicit def T: TraverseTWrapper[N, T]

  def traverseT[M[+_]] = new (({type λ[α] = M[N[α]]})#λ ~> ({type λ[α] = T[M, α]})#λ) {
    def apply[A](a: M[N[A]]): T[M, A] = T.wrap(a)
  }
}

trait TraverseTInstances[N[+_], T[M[+_], +A] <: TraverseT[N, T, M, A]]
    extends TraverseTInstancesMonadTrans[N, T] with TraverseTInstancesMonadPlus[N, T]
