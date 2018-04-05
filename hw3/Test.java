class Test {
  public static void main() {
    try {
      int nThreads = parseInt (args[1], 1, Integer.MAX_VALUE);
			int nTransitions = parseInt (args[2], 0, Integer.MAX_VALUE);
			byte maxval = (byte) parseInt (args[3], 0, 127);
			byte[] value = new byte[args.length - 4];
			for (int i = 4; i < args.length; i++)
				value[i - 4] = (byte) parseInt (args[i], 0, maxval);
			byte[] stateArg = value.clone();
			
			State s;
			if (args[0].equals("Null"))
				s = new NullState(stateArg, maxval);
			else if (args[0].equals("Synchronized"))
				s = new SynchronizedState(stateArg, maxval);
			else if (args[0].equals("Unsynchronized"))
				s = new Unsynchronized(stateArg, maxval);
			else if (args[0].equals("GetNSet"))
				s = new GetNSet(stateArg, maxval);
			else if (args[0].equals("BetterSafe"))
				s = new BetterSafe(stateArg, maxval);
			else
				throw new Exception(args[0]);
			
			dowork(nThreads, nTransitions, s);
			test(value, s.current(), maxval);
			System.exit (0);
		} catch (Exception e) {
			usage(e);
    }
    
    private static int parseInt(String s, int min, int max) {
      int n = Integer.parseInt(s);
      if (n < min || n > max)
          throw new NumberFormatException(s);
      return n;
        }
    
    private static long dowork(int nThreads, int nTransitions, State s)
          throws InterruptedException {
      Thread[] t = new Thread[nThreads];
      for (int i = 0; i < nThreads; i++) {
          int threadTransitions =
        (nTransitions / nThreads
         + (i < nTransitions % nThreads ? 1 : 0));
          t[i] = new Thread (new SwapTest (threadTransitions, s));
      }
      long start = System.nanoTime();
      for (int i = 0; i < nThreads; i++)
          t[i].start ();
      for (int i = 0; i < nThreads; i++)
          t[i].join ();
      long end = System.nanoTime();
      double elapsed_ns = end - start;
      return (elapsed_ns * nThreads / nTransitions);
  }
}