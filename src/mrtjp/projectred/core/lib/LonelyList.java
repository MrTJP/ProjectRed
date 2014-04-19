package mrtjp.projectred.core.lib;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

public class LonelyList<E> implements List<E>
{
    private final E content;

    public LonelyList(E object)
    {
        if (object == null)
            throw new NullPointerException();

        this.content = object;
    }

    @Override
    public boolean add(E e)
    {
        throw new UnsupportedOperationException("Can't mofidy the LonelyList");
    }

    @Override
    public void add(int index, E element)
    {
        throw new UnsupportedOperationException("Can't mofidy the LonelyList");
    }

    @Override
    public boolean addAll(Collection<? extends E> c)
    {
        throw new UnsupportedOperationException("Can't mofidy the LonelyList");
    }

    @Override
    public boolean addAll(int index, Collection<? extends E> c)
    {
        throw new UnsupportedOperationException("Can't mofidy the LonelyList");
    }

    @Override
    public void clear()
    {
        throw new UnsupportedOperationException("Can't mofidy the LonelyList");
    }

    @Override
    public boolean contains(Object o)
    {
        return content.equals(o);
    }

    @Override
    public boolean containsAll(Collection<?> c)
    {
        if (c.size() != 1)
            return false;
        return content.equals(c.iterator().next());
    }

    @Override
    public E get(int index)
    {
        if (index != 0)
            throw new IndexOutOfBoundsException("LonelyList can't access an object at: " + index);
        return content;
    }

    @Override
    public int indexOf(Object o)
    {
        if (content.equals(o))
            return 0;
        return -1;
    }

    @Override
    public boolean isEmpty()
    {
        return false;
    }

    @Override
    public Iterator<E> iterator()
    {
        return new Iterator<E>() {
            private boolean handled = false;

            @Override
            public boolean hasNext()
            {
                return !handled;
            }

            @Override
            public E next()
            {
                if (handled)
                    return null;
                handled = true;
                return LonelyList.this.content;
            }

            @Override
            public void remove()
            {
                throw new UnsupportedOperationException("Can't mofidy the LonelyList");
            }
        };
    }

    @Override
    public int lastIndexOf(Object o)
    {
        return indexOf(o);
    }

    @Override
    public ListIterator<E> listIterator()
    {
        throw new UnsupportedOperationException("LonelyList doesn't have this kind of iterator");
    }

    @Override
    public ListIterator<E> listIterator(int index)
    {
        throw new UnsupportedOperationException("LonelyList doesn't have this kind of iterator");
    }

    @Override
    public boolean remove(Object o)
    {
        throw new UnsupportedOperationException("Can't mofidy the LonelyList");
    }

    @Override
    public E remove(int index)
    {
        throw new UnsupportedOperationException("Can't mofidy the LonelyList");
    }

    @Override
    public boolean removeAll(Collection<?> c)
    {
        throw new UnsupportedOperationException("Can't mofidy the LonelyList");
    }

    @Override
    public boolean retainAll(Collection<?> c)
    {
        throw new UnsupportedOperationException("Can't mofidy the LonelyList");
    }

    @Override
    public E set(int index, E element)
    {
        throw new UnsupportedOperationException("Can't mofidy the LonelyList");
    }

    @Override
    public int size()
    {
        return 1;
    }

    @Override
    public List<E> subList(int fromIndex, int toIndex)
    {
        throw new UnsupportedOperationException();
    }

    @Override
    public Object[] toArray()
    {
        return new Object[] { content };
    }

    @Override
    public <T> T[] toArray(T[] a)
    {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean equals(Object obj)
    {
        if (!(obj instanceof LonelyList))
            return false;
        return content.equals(((LonelyList<?>) obj).content);
    }

    @Override
    public String toString()
    {
        return "[" + content.toString() + "]";
    }
}
