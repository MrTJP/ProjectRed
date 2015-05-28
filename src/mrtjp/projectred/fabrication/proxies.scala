/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import java.lang.{Character => JChar}

import codechicken.lib.data.MCDataInput
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory2
import cpw.mods.fml.client.registry.ClientRegistry
import cpw.mods.fml.common.registry.GameRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.block.TileRenderRegistry
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedFabrication._
import mrtjp.projectred.core.IProxy
import mrtjp.projectred.integration.{GateDefinition, RenderGate}
import net.minecraft.inventory.InventoryCrafting
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.IRecipe
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.world.World
import net.minecraftforge.client.MinecraftForgeClient

class FabricationProxy_server extends IProxy with IPartFactory2
{
    override def preinit()
    {
    }

    override def init()
    {
        icBlock = new BlockICMachine
        icBlock.addTile(classOf[TileICWorkbench], 0)
        icBlock.addTile(classOf[TileICPrinter], 1)

        itemICBlueprint = new ItemICBlueprint
        itemICChip = new ItemICChip

        MultiPartRegistry.registerParts(this, "pr_icgate")

        FabricationRecipes.initRecipes()
    }

    override def postinit(){}

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"

    override def createPart(name:String, nbt:NBTTagCompound) = createPart(name)
    override def createPart(name:String, packet:MCDataInput) = createPart(name)

    def createPart(name:String) = name match
    {
        case "pr_icgate" => new CircuitGatePart
        case _ => null
    }
}

class FabricationProxy_client extends FabricationProxy_server
{
    val icWorkbenchGui = 12
    val icPrinterGui = 13

    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()

        TileRenderRegistry.setRenderer(icBlock, 0, RenderICWorkbench)
        TileRenderRegistry.setRenderer(icBlock, 1, RenderICPrinter)

        MinecraftForgeClient.registerItemRenderer(itemICBlueprint, ItemRenderICBlueprint)

        ClientRegistry.bindTileEntitySpecialRenderer(classOf[TileICPrinter], RenderICPrinterDynamic)

        GuiHandler.register(GuiICWorkbench, icWorkbenchGui)
        GuiHandler.register(GuiICPrinter, icPrinterGui)

        RenderGate.hotswap(new RenderCircuitGate, GateDefinition.ICGate.ordinal)
    }
}

object FabricationProxy extends FabricationProxy_client

object FabricationRecipes
{
    def initRecipes()
    {
        GameRegistry.addRecipe(new IRecipe {

            override def matches(inv:InventoryCrafting, w :World) =
                getCraftingResult(inv) != null

            override def getRecipeOutput = GateDefinition.ICGate.makeStack

            override def getRecipeSize = 2

            override def getCraftingResult(inv:InventoryCrafting) =
            {
                val bp = findBP(inv)
                if (bp != null && ItemICBlueprint.hasICInside(bp))
                {
                    val out = getRecipeOutput
                    ItemICBlueprint.copyToGate(bp, out)
                    out
                }
                else null
            }

            def findBP(inv:InventoryCrafting):ItemStack =
            {
                for (i <- 0 until inv.getSizeInventory)
                {
                    val stack = inv.getStackInSlot(i)
                    if (stack != null && stack.getItem.isInstanceOf[ItemICBlueprint])
                        return stack
                }
                null
            }
        })
    }
}