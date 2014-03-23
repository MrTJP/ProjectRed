package mrtjp.projectred.transportation

import java.util.concurrent.PriorityBlockingQueue

object TableUpdateThread
{
    private val updateCalls = new PriorityBlockingQueue[RouteLayerUpdater]()
    private var average = 0L

    val avgSync = new AnyRef

    def add(r:Router)
    {
        updateCalls.add(new RouteLayerUpdater(r))
    }

    def remove(run:Runnable) = updateCalls.remove(run)

    def size = updateCalls.size

    def getAverage:Long =
    {
        avgSync synchronized
            {
                return average
            }
    }
}

class TableUpdateThread(i:Int) extends Thread("PR RoutingThread #"+i)
{
    setDaemon(true)
    setPriority(Thread.NORM_PRIORITY)
    start()

    override def run()
    {
        while (true) try
        {
            while (!TableUpdateThread.updateCalls.isEmpty)
            {
                val rlu = TableUpdateThread.updateCalls.poll()
                val starttime = System.nanoTime
                if (rlu != null) rlu.run()
                val took = System.nanoTime-starttime

                TableUpdateThread.avgSync synchronized
                    {
                        if (TableUpdateThread.average == 0) TableUpdateThread.average = took
                        else TableUpdateThread.average = (TableUpdateThread.average*999L+took)/1000L
                    }
            }
        }
        catch {case e:InterruptedException =>}
    }
}

private[this] class RouteLayerUpdater(val router:Router) extends Runnable with Ordered[RouteLayerUpdater]
{
    private val newVersion = router.getLinkStateID
    private var complete = false

    def run()
    {
        if (complete) return
        try
        {
            if (router.getParent == null) return
            {
                var i = 0
                while (i < 10 && !router.isLoaded)
                {
                    Thread.sleep(10)
                    i += 1
                }
            }
            if (!router.isLoaded) return
            router.refreshRoutingTable(newVersion)
        }
        catch {case e:Exception => e.printStackTrace()}
        complete = true
    }

    override def compare(that:RouteLayerUpdater):Int =
    {
        var c = 0
        if (that.newVersion <= 0) c = newVersion-that.newVersion
        if (c != 0) return c
        c = router.getIPAddress-that.router.getIPAddress
        if (c != 0) return c
        c = that.newVersion-newVersion
        c
    }
}