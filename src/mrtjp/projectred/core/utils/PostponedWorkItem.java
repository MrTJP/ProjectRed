package mrtjp.projectred.core.utils;

import java.util.concurrent.Delayed;
import java.util.concurrent.TimeUnit;

/**
 * @author Andrey Redko - http://aredko.blogspot.com
 */
public class PostponedWorkItem<T> implements Delayed
{
    private final long origin;
    private final long delay;
    private final T workItem;

    public PostponedWorkItem(final T workItem, final long delay)
    {
        this.origin = System.currentTimeMillis();
        this.workItem = workItem;
        this.delay = delay;
    }

    public T getItem()
    {
        return workItem;
    }

    @Override
    public long getDelay(TimeUnit unit)
    {
        return unit.convert(delay - (System.currentTimeMillis() - origin), TimeUnit.MILLISECONDS);
    }

    @Override
    public int compareTo(Delayed delayed)
    {
        if (delayed == this)
            return 0;

        if (delayed instanceof PostponedWorkItem)
        {
            long diff = delay - ((PostponedWorkItem) delayed).delay;
            return ((diff == 0) ? 0 : ((diff < 0) ? -1 : 1));
        }

        long d = (getDelay(TimeUnit.MILLISECONDS) - delayed.getDelay(TimeUnit.MILLISECONDS));
        return ((d == 0) ? 0 : ((d < 0) ? -1 : 1));
    }
}
