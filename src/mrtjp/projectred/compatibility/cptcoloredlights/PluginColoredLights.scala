/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.compatibility.cptcoloredlights

import mrtjp.core.color.Colors
import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.illumination.IlluminationProxy

object PluginColoredLights extends IPRPlugin
{
    val PRIll_modID = "ProjRed|Illumination"
    val CL_modID = "easycoloredlights"

    override def getModIDs = Array(CL_modID, PRIll_modID)

    override def preInit()
    {
        if (Configurator.coloredLightsCompat)
            IlluminationProxy.getLightValue = (m, b) =>
            {
                if (!(0 until 16 contains m)) b
                else
                {
                    val c = Colors(m)
                    CLCAPIProxy.makeRGBLightValue((c.rF*15).toInt, (c.gF*15).toInt, (c.bF*15).toInt, b)
                }
            }
    }

    override def init(){}
    override def postInit(){}

    override def desc() = "CPT Colored Lights Compat for Illumination lighting"
}
