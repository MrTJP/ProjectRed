package mrtjp.projectred.expansion

import mrtjp.projectred.core.{BasicRenderUtils, Configurator, IProxy}
import mrtjp.projectred.core.blockutil.ItemBlockMulti
import mrtjp.projectred.{ExpansionSPH, ExpansionCPH, ProjectRedExpansion}

import codechicken.multipart.MultiPartRegistry.IPartFactory
import codechicken.multipart.TMultiPart
import cpw.mods.fml.relauncher.{Side, SideOnly}
import cpw.mods.fml.common.registry.GameRegistry
import codechicken.lib.packet.PacketCustom
import mrtjp.projectred.transportation.TransportationSPH
import codechicken.microblock.MicroMaterialRegistry

class ExpansionProxy_server extends IProxy
{
    def preinit() = PacketCustom.assignHandler(ExpansionSPH.channel, ExpansionSPH)

    def init()
    {
        ProjectRedExpansion.machine1 = new BlockMachine(Configurator.block_machinesID.getInt)
        ProjectRedExpansion.machine1.setUnlocalizedName("projectred.expansion.machine1")
        GameRegistry.registerBlock(ProjectRedExpansion.machine1, classOf[ItemBlockMulti], "projectred.expansion.machine1")
        ProjectRedExpansion.machine1.addTile(0, classOf[TileElectricFurnace], "projectred.expansion.machine1.furnace")


    }

    def postinit() = ExpansionRecipes.initRecipes()
}

class ExpansionProxy_client extends ExpansionProxy_server
{
    @SideOnly(Side.CLIENT)
    override def preinit() = PacketCustom.assignHandler(ExpansionCPH.channel, ExpansionCPH)

    @SideOnly(Side.CLIENT)
    override def init() =
    {
        super.init()

        BasicRenderUtils.setRenderer(ProjectRedExpansion.machine1, 0, TileElectricFurnaceRenderer)
    }
}

object ExpansionProxy extends ExpansionProxy_client