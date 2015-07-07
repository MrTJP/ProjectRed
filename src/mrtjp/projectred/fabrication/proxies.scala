/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication
import java.lang.{Character => JC}
import codechicken.lib.data.MCDataInput
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory2
import cpw.mods.fml.client.registry.ClientRegistry
import cpw.mods.fml.common.registry.GameRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.block.TileRenderRegistry
import mrtjp.core.color.Colors
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.{ProjectRedIntegration, ProjectRedFabrication}
import mrtjp.projectred.ProjectRedFabrication._
import mrtjp.projectred.core.{IProxy, PartDefs}
import mrtjp.projectred.integration.{GateDefinition, RenderGate}
import net.minecraft.init.{Items, Blocks}
import net.minecraft.inventory.InventoryCrafting
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.IRecipe
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.world.World
import net.minecraftforge.client.MinecraftForgeClient
import net.minecraftforge.oredict.{ShapelessOreRecipe, ShapedOreRecipe}

class FabricationProxy_server extends IProxy with IPartFactory2
{
    override def preinit(){}

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

    override def postinit()
    {
        //hook into gate part to add tooltip info
        ProjectRedIntegration.itemPartGate2.infoBuilderFunc = {(stack, list) =>
            if (stack.getItemDamage == GateDefinition.ICGate.meta)
                ItemICChip.addInfo(stack, list)
        }
    }

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
        //IC Gate recipe
        GameRegistry.addRecipe(new IRecipe {
            override def matches(inv:InventoryCrafting, w:World) = getCraftingResult(inv) != null

            override def getRecipeOutput = GateDefinition.ICGate.makeStack

            override def getRecipeSize = 9

            override def getCraftingResult(inv:InventoryCrafting):ItemStack =
            {
                for (i <- 0 until 9)
                {
                    val stack = inv.getStackInSlot(i)
                    if (stack == null) return null
                    i match
                    {
                        case 4 => if (stack.getItem != ProjectRedFabrication.itemICChip ||
                                !ItemICBlueprint.hasICInside(stack)) return null
                        case _ => if (!stack.isItemEqual(PartDefs.PLATE.makeStack)) return null
                    }
                }
                val out = GateDefinition.ICGate.makeStack
                ItemICBlueprint.copyToGate(inv.getStackInSlot(4), out)
                out
            }
        })

        //IC Workbench
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(icBlock, 1, 0),
            "iii","www","www",
            'i':JC, "blockIron",
            'w':JC, "plankWood"
        ))

        //IC Printer
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(icBlock, 1, 1),
            "ggg","oeo","iwi",
            'g':JC, new ItemStack(Blocks.stained_glass, 1, Colors.LIGHT_BLUE.woolID),
            'o':JC, Blocks.obsidian,
            'e':JC, "gemEmerald",
            'i':JC, "ingotIron",
            'w':JC, "plankWood"
        ))

        //IC Blueprint
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemICBlueprint),
            "pbp","brb","pbp",
            'p':JC, Items.paper,
            'b':JC, "dyeBlue",
            'r':JC, Items.redstone
        ))

        //IC Blueprint - reset
        GameRegistry.addRecipe(new IRecipe {
            override def matches(inv:InventoryCrafting, w:World) = getCraftingResult(inv) != null

            override def getRecipeOutput = new ItemStack(itemICBlueprint)

            override def getRecipeSize = 2

            override def getCraftingResult(inv:InventoryCrafting):ItemStack =
            {
                var bp:ItemStack = null
                for (i <- 0 until inv.getSizeInventory)
                {
                    val s = inv.getStackInSlot(i)
                    if (s != null)
                        if (bp != null) return null
                        else bp = s
                }

                if (bp != null && bp.getItem == itemICBlueprint && ItemICBlueprint.hasICInside(bp))
                    new ItemStack(itemICBlueprint)
                else null
            }
        })

        //IC Blueprint - copy
        GameRegistry.addRecipe(new IRecipe {
            override def matches(inv:InventoryCrafting, w:World) = getCraftingResult(inv) != null

            override def getRecipeOutput = new ItemStack(itemICBlueprint, 2)

            override def getRecipeSize = 2

            override def getCraftingResult(inv:InventoryCrafting):ItemStack =
            {
                var bp:ItemStack = null
                var empty:ItemStack = null
                for (i <- 0 until inv.getSizeInventory)
                {
                    val s = inv.getStackInSlot(i)
                    if (s != null)
                    {
                        if (s.getItem != itemICBlueprint) return null
                        if (ItemICBlueprint.hasICInside(s))
                            if (bp != null) return null
                            else bp = s
                        else
                            if (empty != null) return null
                            else empty = s
                    }
                }
                if (bp != null && empty != null)
                {
                    val out = new ItemStack(itemICBlueprint)
                    out.stackSize = 2
                    ItemICBlueprint.copyIC(bp, out)
                    out
                }
                else null
            }

            def countEmptyBlueprints(inv:InventoryCrafting) =
            {
                var count = 0
                for (i <- 0 until inv.getSizeInventory)
                {
                    val s = inv.getStackInSlot(i)
                    if (s != null && s.getItem.isInstanceOf[ItemICBlueprint] &&
                            !ItemICBlueprint.hasICInside(s))
                            count += 1
                }
                count
            }
        })

        //IC Chip
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemICChip),
            "ggg","qdq", "ggg",
            'g':JC, "nuggetGold",
            'q':JC, "gemQuartz",
            'd':JC, "gemDiamond"
        ))
    }
}