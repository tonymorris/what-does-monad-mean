using System;
using System.Collections.Generic;
using System.Linq;

static class MonadFunctions {
  public static IEnumerable<A> joinEnumerable<A>(IEnumerable<IEnumerable<A>> a) {
    // Forced repetition.
    return a.SelectMany(z => z);
  }

  public static IQueryable<A> joinQueryable<A>(IQueryable<IQueryable<A>> a) {
    // Repeats for each type constructor.
    return a.SelectMany(z => z);
  }

  // join*
}

class M {
  public static void Main(string[] args) {
    var i = new[]{1, 2, 3};
    var j = new[]{4, 5, 6};
    Print(MonadFunctions.joinEnumerable(new[]{i as IEnumerable<int>, j}));
  }

  static void Print<A>(IEnumerable<A> a) {
    foreach(A i in a)
      Console.Write("{0} ", i);
    Console.WriteLine();
  }
}
