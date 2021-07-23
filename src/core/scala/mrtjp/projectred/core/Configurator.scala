/*
 * Copyright (c) 2016.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.core

import mrtjp.core.data.ModConfig

object Configurator extends ModConfig("projectred-core") {
    /** Constants * */
    var modName = "Project Red"

    /** General Settings * */
    var logicGateSounds = true
    var minTimerTicks = 4
    var simpleFramedWireRecipe = false
    var unbreakableScrewdriver = false

    var maxDetectionCount = 100
    var detectionFrequency = 40
    var routerUpdateThreadCount = 4
    var maxPipesWandered = 0

    /** Machines * */
    var enableDiamondBlockBreaker = false

    /** Rendering * */
    var logicwires3D = true
    var staticWires = true
    var staticGates = true
    var lightHaloMax = -1
    var pipeRoutingFX = true

    /** World Gen * */
    var gen_MarbleCave = true
    //    var gen_MarbleCave_resistance = 0
    //    var gen_MarbleCave_retro = false
    //    var gen_Volcano = true
    //    var gen_Volcano_resistance = 0
    //    var gen_Volcano_retro = false
    var gen_Ruby = true
    //    var gen_Ruby_resistance = 0
    //    var gen_Ruby_retro = false
    var gen_Sapphire = true
    //    var gen_Sapphire_resistance = 0
    //    var gen_Sapphire_retro = false
    var gen_Peridot = true
    //    var gen_Peridot_resistance = 0
    //    var gen_Peridot_retro = false
    var gen_Copper = true
    //    var gen_Copper_resistance = 0
    //    var gen_Copper_retro = false
    var gen_Tin = true
    //    var gen_Tin_resistance = 0
    //    var gen_Tin_retro = false
    var gen_Silver = true
    //    var gen_Silver_resistance = 0
    //    var gen_Silver_retro = false
    var gen_Electrotine = true
    //    var gen_Electrotine_resistance = 0
    //    var gen_Electrotine_retro = false

    /** Compatibility * */
    //    var compat_Treecapitator = true
    //    var compat_CCBundledCalbe = true
    //    var compat_Chisel = true
    //    var compat_TConstruct = true
    //    var compat_TExpansion = true
    //    var compat_ColoredLights = false
    //    var compat_MFRDeepStorage = true

    override def getFileName = "ProjectRed"

    override protected def initValues() {
        val general = new BaseCategory("General Settings", "Contains basic settings for the mod.")

        logicGateSounds = general.put("Logic Sounds", logicGateSounds, "If set to false, logic gates will not make sounds.")
        minTimerTicks = general.put("Minimum Timer Ticks", minTimerTicks, "Minimum amount of ticks the timer gates can be set to. Cannot be lower than 4.")
        simpleFramedWireRecipe = general.put("Simple Framed Wire Recipe", simpleFramedWireRecipe, "If set to true, sticks will be used instead of wood strips in framed wire recipes.")
        unbreakableScrewdriver = general.put("Unbreakable Screwdriver", unbreakableScrewdriver, "If set to true, the basic screwdriver will not take damage.")


        maxDetectionCount = general.put("Routed Pipes: Max Detection Count", maxDetectionCount, "Max number of pipes to explore when connecting to other routers.")
        detectionFrequency = general.put("Routed Pipes: Detection Frequency", detectionFrequency, "Ticks between router searches.")
        routerUpdateThreadCount = general.put("Routed Pipes: Update Threads", routerUpdateThreadCount, "Number of active route table update threads.")
        maxPipesWandered = general.put("Routed Pipes: Max Wander Distance", maxPipesWandered, "Maximum distance an item may aimlessly wander in a pipe before being erased. 0 for unlimited.")

        val machines = new BaseCategory("Machine Settings", "Contains settings related to machines and devices.")
        enableDiamondBlockBreaker = machines.put("Enable the Diamond Block Breaker", enableDiamondBlockBreaker, "Allow the Diamond Block Breaker to be crafted.")

        val rendering = new BaseCategory("Render Settings", "Contains settings related to how things render in-game.")
        logicwires3D = rendering.put("3D Logic Wires", logicwires3D, "If set to false, flat wire textures will be used for logic gates. Significant performance improvement.")
        staticWires = rendering.put("Static Wires", staticWires, "If set to false, wires will be rendered in the TESR rather than the WorldRenderer.")
        staticGates = rendering.put("Static Gates", staticGates, "If set to false, gates will be rendered in the TESR rather than the WorldRenderer.")
        lightHaloMax = rendering.put("Light Halo Render Count", lightHaloMax, "Number of lights to render, -1 for unlimited")
        pipeRoutingFX = rendering.put("Routed Pipe FX", pipeRoutingFX, "If set to false, routed pipes will not render routing fx such as bubbles and lasers.")

        val gen = new BaseCategory("World Gen", "Contains settings related to world gen. You can enable/disable each ore or strucure, change retro generation settings, and increase how rare something is by increasing the resistance value.")
        gen_Ruby = gen.put("Ruby Ore", gen_Ruby)
        //        gen_Ruby_resistance = gen.put("Ruby Ore resistance", gen_Ruby_resistance)
        //        gen_Ruby_retro = gen.put("Ruby Ore retrogen", gen_Ruby_retro)
        //
        gen_Sapphire = gen.put("Sapphire Ore", gen_Sapphire)
        //        gen_Sapphire_resistance = gen.put("Sapphire Ore resistance", gen_Sapphire_resistance)
        //        gen_Sapphire_retro = gen.put("Sapphire Ore retrogen", gen_Sapphire_retro)
        //
        gen_Peridot = gen.put("Peridot Ore", gen_Peridot)
        //        gen_Peridot_resistance = gen.put("Peridot Ore resistance", gen_Peridot_resistance)
        //        gen_Peridot_retro = gen.put("Peridot Ore retrogen", gen_Peridot_retro)
        //
        gen_MarbleCave = gen.put("Marble Caves", gen_MarbleCave)
        //        gen_MarbleCave_resistance = gen.put("Marble Caves resistance", gen_MarbleCave_resistance)
        //        gen_MarbleCave_retro = gen.put("Marble Caves retrogen", gen_MarbleCave_retro)
        //
        //        gen_Volcano = gen.put("Volcanos", gen_Volcano)
        //        gen_Volcano_resistance = gen.put("Volcano resistance", gen_Volcano_resistance)
        //        gen_Volcano_retro = gen.put("Volcano retrogen", gen_Volcano_retro)
        //
        gen_Copper = gen.put("Copper Ore", gen_Copper)
        //        gen_Copper_resistance = gen.put("Copper Ore resistance", gen_Copper_resistance)
        //        gen_Copper_retro = gen.put("Copper Ore retrogen", gen_Copper_retro)
        //
        gen_Tin = gen.put("Tin Ore", gen_Tin)
        //        gen_Tin_resistance = gen.put("Tin Ore resistance", gen_Tin_resistance)
        //        gen_Tin_retro = gen.put("Tin Ore retrogen", gen_Tin_retro)
        //
        gen_Silver = gen.put("Silver Ore", gen_Silver)
        //        gen_Silver_resistance = gen.put("Silver Ore resistance", gen_Silver_resistance)
        //        gen_Silver_retro = gen.put("Silver Ore retrogen", gen_Silver_retro)
        //
        gen_Electrotine = gen.put("Electrotine Ore", gen_Electrotine)
        //        gen_Electrotine_resistance = gen.put("Electrotine Ore resistance", gen_Electrotine_resistance)
        //        gen_Electrotine_retro = gen.put("Electrotine Ore retrogen", gen_Electrotine_retro)

        //        val compat = new BaseCategory("Compatibility", "Control the loading of various compatibility hooks. These settings are ignored unless the Compatibility module is installed.")
        //        compat_Treecapitator = compat.put("Treecapitator: Gem Axe", compat_Treecapitator, "This allows gem axes to work with treecapitator.")
        //        compat_CCBundledCalbe = compat.put("ComputerCraft: Bundled Cables", compat_CCBundledCalbe, "This allows computers to connect to bundled cables with the RS API.")
        //        compat_Chisel = compat.put("Chisel: Decorative Blocks", compat_Chisel, "Registers ProjectRed decorative blocks with Chisel.")
        //        compat_TConstruct = compat.put("Tinkers Construct: Smeltery", compat_TConstruct, "This adds recipes to the smeltery.")
        //        compat_TExpansion = compat.put("Thermal Expansion: Machine Recipes", compat_TExpansion, "This adds recipes to machines.")
        //        compat_ColoredLights = compat.put("ColoredLights Compat", compat_ColoredLights, "This makes things emit colored light. CLC is in beta state and may cause minor rendering glitches.")
        //        compat_MFRDeepStorage = compat.put("MFR: Deep Storage", compat_MFRDeepStorage, "This allows pipes to recoginze MFR Deep storage units correctly.")
    }
}
