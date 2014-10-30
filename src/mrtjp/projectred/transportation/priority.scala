package mrtjp.projectred.transportation

import mrtjp.core.color.Colors
import mrtjp.core.util.Enum

object Priorities extends Enum
{
    override type EnumVal = Priority
    type NetworkPriority = EnumVal

    val passiveDef = { path:StartEndPath => path.allowRouting}
    val activeDef = { path:StartEndPath => path.allowBroadcast || path.allowCrafting}

    val WANDERING = new Priority("Wandering", 0.02f, 0.05f, Colors.RED.ordinal, passiveDef)
    val DEFAULT = new Priority("Default", 0.05f, 0.10f, Colors.ORANGE.ordinal(), passiveDef)
    val TERMINATED = new Priority("Terminated", 0.02f, 0.05f, Colors.PURPLE.ordinal(), passiveDef)
    val PASSIVE = new Priority("Passive", 0.10f, 0.20f, Colors.BLUE.ordinal(), passiveDef)
    val ACTIVEB = new Priority("Active Broadcast", 0.20f, 0.30f, Colors.GREEN.ordinal(), _.allowBroadcast)
    val ACTIVEC = new Priority("Active Craft", 0.20f, 0.30f, Colors.GREEN.ordinal(), _.allowCrafting)

    class Priority(val ident:String, val speed:Float, val boost:Float, val color:Int, f:StartEndPath => Boolean) extends Value
    {
        override def name = ident

        /**
         * Used to check if a particular router can route to another on this priority
         * with the given path. This should see if said path does not restrict this
         * priority. (Item checks are done on the fly, ignore them)
         * @param path The path to check routing for
         * @return True if this priority can route using given path.
         */
        def isPathUsable(path:StartEndPath) = f(path)
    }
}