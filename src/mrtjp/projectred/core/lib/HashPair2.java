package mrtjp.projectred.core.lib;

public class HashPair2<T1, T2>
{
    private final T1 val1;
    private final T2 val2;
    private final int hashCode;

    public HashPair2(T1 value1, T2 value2)
    {
        this.val1 = value1;
        this.val2 = value2;
        hashCode = value1.hashCode()^value2.hashCode();
    }

    public T1 get1()
    {
        return val1;
    }

    public T2 get2()
    {
        return val2;
    }

    @Override
    public int hashCode()
    {
        return hashCode;
    }

    public HashPair2<T1, T2> copy()
    {
        return new HashPair2<T1, T2>(val1, val2);
    }
}