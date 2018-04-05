import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State {
  private AtomicIntegerArray arr;
  private byte maxval;

  GetNSet(byte[] v) {
    arr = new AtomicIntegerArray(v.length);
    for (int i = 0; i < v.length; i++) {
      arr.set(i, v[i]);
    }
    maxval = 127;
  }

  GetNSet(byte[] v, byte m) { 
    arr = new AtomicIntegerArray(v.length);
    for (int i = 0; i < v.length; i++) {
      arr.set(i, v[i]);
    }
    maxval = m;
  }

  public int size() { return arr.length(); }

  public byte[] current() {
    int n = arr.length();
    byte[] b = new byte[n];
    for (int i = 0; i < n; i++)
      b[i] = (byte) arr.get(i); 
    return b; 
  }

  public boolean swap(int i, int j) {
    if (arr.get(i) <= 0 || arr.get(j) >= maxval) {
        return false;
    }
    arr.set(i, arr.get(i) - 1);
    arr.set(j, arr.get(j) + 1);
    return true;
  }
}
