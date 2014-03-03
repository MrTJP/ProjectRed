package mrtjp.projectred.transportation

import scala.collection.mutable

object TableUpdateThread
{
    private var updateCalls = new mutable.PriorityQueue[RouteLayerUpdater]
    private var average = 0L

    val avgSync = new AnyRef

    def add(r:Router)
    {
        updateCalls += new RouteLayerUpdater(r)
    }

    def remove(run:Runnable) = updateCalls.filterNot(p => p == run)

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
        try
        {
            while (!TableUpdateThread.updateCalls.isEmpty)
            {
                val rlu = TableUpdateThread.updateCalls.dequeue()
                val starttime = System.nanoTime
                rlu.run()
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

class RouteLayerUpdater(val router:Router) extends Ordered[RouteLayerUpdater] with Runnable
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

    override def compare(o:RouteLayerUpdater):Int =
    {
        var c = 0
        if (o.newVersion <= 0) c = newVersion-o.newVersion
        if (c != 0) return c
        c = router.getIPAddress-o.router.getIPAddress
        if (c != 0) return c
        c = o.newVersion-newVersion
        c
    }


}