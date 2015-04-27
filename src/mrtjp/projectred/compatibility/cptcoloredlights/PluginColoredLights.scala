/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.compatibility.cptcoloredlights

import coloredlightscore.src.api.CLApi
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
                    CLApi.makeRGBLightValue(c.rF, c.gF, c.bF)
                }
            }
    }

    override def init(){}
    override def postInit(){}

    override def desc() = "CPT Colored Lights Compat for Illumination lighting"
}
