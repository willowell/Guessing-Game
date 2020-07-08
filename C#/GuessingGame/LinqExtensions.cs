using System;

/**
 * Used with permission from https://gist.github.com/johnazariah/d95c03e2c56579c11272a647bab4bc38
 */

namespace Maybe {
	public static partial class LinqExtensions {
		public static Maybe<C> SelectMany<A, B, C>(this Maybe<A> ma, Func<A, Maybe<B>> f, Func<A, B, C> select) => ma.Bind(a => f(a).Map(b => select(a, b)));
	}
}
