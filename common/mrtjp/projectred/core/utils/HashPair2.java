package mrtjp.projectred.core.utils;

public class HashPair2<T1, T2> {
    private final T1 value1;
    private final T2 value2;
    private final int hashCode;
    
    public HashPair2(T1 value1, T2 value2) {
        this.value1 = value1;
        this.value2 = value2;
        hashCode = value1.hashCode() ^ value2.hashCode();
    }

    public T1 getValue1() {
        return value1;
    }
    
    public T2 getValue2() {
        return value2;
    }
    
    
    @Override
    public int hashCode() {
        return hashCode;
    }

    public HashPair2<T1, T2> copy() {
        return new HashPair2<T1, T2>(value1, value2);
    }
}
