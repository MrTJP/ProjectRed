/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import java.lang.{Character => JChar}

import codechicken.lib.model.ModelRegistryHelper
import codechicken.lib.packet.PacketCustom
import codechicken.lib.texture.TextureUtils
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.api.IPartFactory
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedIntegration._
import mrtjp.projectred.core.IProxy
import net.minecraft.util.ResourceLocation
import net.minecraftforge.fml.common.registry.ForgeRegistries
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class IntegrationProxy_server extends IProxy with IPartFactory
{
    override def preinit()
    {
        itemPartGate = new ItemPartGate
        itemPartGate.setUnlocalizedName("projectred.integration.gate")
        ForgeRegistries.ITEMS.register(itemPartGate.setRegistryName("gate"))

        import GateDefinition._
        MultiPartRegistry.registerParts(this, Array(
            typeSimpleGate, typeComplexGate, typeArrayGate,
            typeBundledGate, typeNeighborGate
        ))
    }

    override def init()
    {
        PacketCustom.assignHandler(IntegrationSPH.channel, IntegrationSPH)
    }

    override def postinit(){}

    override def createPart(name:ResourceLocation, client:Boolean) = name match
    {
        case GateDefinition.typeSimpleGate => new ComboGatePart
        case GateDefinition.typeComplexGate => new SequentialGatePart
        case GateDefinition.typeArrayGate => new ArrayGatePart
        case GateDefinition.typeBundledGate => new BundledGatePart
        case GateDefinition.typeNeighborGate => new SequentialGatePartT
        case _ => null
    }
}

class IntegrationProxy_client extends IntegrationProxy_server
{
    val timerGui = 10
    val counterGui = 11

    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()

        ModelRegistryHelper.registerItemRenderer(itemPartGate, GateItemRenderer)
        TextureUtils.addIconRegister(RenderGate)
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()

        PacketCustom.assignHandler(IntegrationCPH.channel, IntegrationCPH)

        GuiHandler.register(GuiTimer, timerGui)
        GuiHandler.register(GuiCounter, counterGui)
    }
}

object IntegrationProxy extends IntegrationProxy_client
