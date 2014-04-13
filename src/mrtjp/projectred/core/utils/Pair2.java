package mrtjp.projectred.core.utils;

public class Pair2<T1, T2>
{
    private T1 val1;
    private T2 val2;

    public Pair2(T1 val1, T2 value2)
    {
        this.val1 = val1;
        this.val2 = value2;
    }

    public T1 get1()
    {
        return val1;
    }

    public T2 get2()
    {
        return val2;
    }

    public void set1(T1 value1)
    {
        val1 = value1;
    }

    public void set2(T2 value2)
    {
        val2 = value2;
    }

    public Pair2<T1, T2> copy()
    {
        return new Pair2<T1, T2>(val1, val2);
    }
}