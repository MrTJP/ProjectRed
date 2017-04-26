/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication
import java.lang.{Character => JC}

import codechicken.lib.colour.EnumColour
import codechicken.lib.model.ModelRegistryHelper
import codechicken.lib.model.blockbakery.{CCBakeryModel, IBlockStateKeyGenerator}
import codechicken.lib.render.item.map.MapRenderRegistry
import codechicken.lib.texture.TextureUtils
import codechicken.lib.texture.TextureUtils.IIconRegister
import codechicken.multipart.{IPartFactory, MultiPartRegistry}
import mrtjp.core.block.{ItemBlockCore, MultiTileBlock}
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedFabrication._
import mrtjp.projectred.core.{IProxy, PartDefs}
import mrtjp.projectred.integration.{GateDefinition, RenderGate}
import mrtjp.projectred.{ProjectRedFabrication, ProjectRedIntegration}
import net.minecraft.block.Block
import net.minecraft.client.renderer.ItemMeshDefinition
import net.minecraft.client.renderer.block.model.ModelResourceLocation
import net.minecraft.client.renderer.block.statemap.IStateMapper
import net.minecraft.client.renderer.block.statemap.StateMap.Builder
import net.minecraft.init.{Blocks, Items}
import net.minecraft.inventory.InventoryCrafting
import net.minecraft.item.crafting.IRecipe
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.world.World
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.common.ForgeHooks
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.client.registry.ClientRegistry
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import net.minecraftforge.oredict.ShapedOreRecipe

class FabricationProxy_server extends IProxy with IPartFactory
{
    override def preinit()
    {
        icBlock = new BlockICMachine(icMachineBakery)
        icBlock.setUnlocalizedName("projectred.fabrication.icMachine")
        GameRegistry.register(icBlock.setRegistryName("ic_machine"))
        GameRegistry.register(new ItemBlockCore(icBlock).setRegistryName(icBlock.getRegistryName))
        icBlock.addTile(classOf[TileICWorkbench], 0)
        icBlock.addTile(classOf[TileICPrinter], 1)

        itemICBlueprint = new ItemICBlueprint
        itemICBlueprint.setUnlocalizedName("projectred.fabrication.icBlueprint")
        GameRegistry.register(itemICBlueprint.setRegistryName("ic_blueprint"))

        itemICChip = new ItemICChip
        itemICChip.setUnlocalizedName("projectred.fabrication.icChip")
        GameRegistry.register(itemICChip.setRegistryName("ic_chip"))

        MultiPartRegistry.registerParts(this, Array("pr_icgate"))

        FabricationRecipes.initRecipes()
    }

    override def init(){}

    override def postinit()
    {
        //hook into gate part to add tooltip info
        ProjectRedIntegration.itemPartGate.infoBuilderFunc = {(stack, list) =>
            if (stack.getItemDamage == GateDefinition.ICGate.meta)
                ItemICChip.addInfo(stack, list)
        }
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"

    /**
      * Create a new instance of the part with the specified type name identifier
      *
      * @param client If the part instance is for the client or the server
      */
    override def createPart(name:String, client:Boolean) = name match
    {
//        case "pr_icgate" => new CircuitGatePart
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

        import BlockICMachine._

        icMachineBakery.registerSubBakery(0, RenderICWorkbench, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val hasBP = state.getValue(UNLISTED_HAS_BP_PROPERTY)
                state.getBlock.getRegistryName.toString + s",bp=$hasBP"
            }
        })

        icMachineBakery.registerSubBakery(1, RenderICPrinter, new IBlockStateKeyGenerator {
            override def generateKey(state:IExtendedBlockState) = {
                val rot = state.getValue(UNLISTED_ROTATION_PROPERTY)
                state.getBlock.getRegistryName.toString + s",rot=$rot"
            }
        })

        registerBlockToBakery(icBlock, icMachineBakery.registerKeyGens(icBlock), new Builder().ignore(MultiTileBlock.TILE_INDEX).build())

        registerModelType(itemICBlueprint, "projectred:fabrication/items", "ic_blueprint")
        MapRenderRegistry.registerMapRenderer(itemICBlueprint, ItemRenderICBlueprint)

        registerModelType(itemICChip, "projectred:fabrication/items", { stack =>
            if (ItemICBlueprint.hasICInside(stack)) "ic_active" else "ic_inert" })

        ClientRegistry.bindTileEntitySpecialRenderer(classOf[TileICPrinter], RenderICPrinterDynamic)

        GuiHandler.register(GuiICWorkbench, icWorkbenchGui)
        GuiHandler.register(GuiICPrinter, icPrinterGui)

//        RenderGate.hotswap(new RenderCircuitGate, GateDefinition.ICGate.ordinal) //TODO
    }

    @SideOnly(Side.CLIENT)
    def registerBlockToBakery(block:Block, iconRegister:IIconRegister, stateMap:IStateMapper) =
    {
        val model = new CCBakeryModel("")
        val regLoc = block.getRegistryName
        ModelLoader.setCustomStateMapper(block, stateMap)
        ModelLoader.setCustomMeshDefinition(Item.getItemFromBlock(block), new ItemMeshDefinition {
            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation(regLoc, "normal")
        })
        ModelRegistryHelper.register(new ModelResourceLocation(regLoc, "normal"), model)
        if (iconRegister != null) {
            TextureUtils.addIconRegister(iconRegister)
        }
    }

    @SideOnly(Side.CLIENT)
    def registerModelType(item:Item, jsonLocation:String, typeValue:String)
    {
        registerModelType(item, 0, jsonLocation, typeValue)
    }

    @SideOnly(Side.CLIENT)
    def registerModelType(item:Item, meta:Int, jsonLocation:String, typeValue:String)
    {
        val modelLoc = new ModelResourceLocation(jsonLocation, "type=" + typeValue)
        ModelLoader.setCustomModelResourceLocation(item, meta, modelLoc)
    }

    @SideOnly(Side.CLIENT)
    def registerModelType(item:Item, jsonLocation:String, typeValue:ItemStack => String)
    {
        ModelLoader.setCustomMeshDefinition(item, new ItemMeshDefinition {
            override def getModelLocation(s:ItemStack) =
                new ModelResourceLocation(jsonLocation, "type=" + typeValue(s))
        })
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
//                ItemICBlueprint.copyToGate(inv.getStackInSlot(4), out) todo
                out
            }

            override def getRemainingItems(inv:InventoryCrafting) = ForgeHooks.defaultRecipeGetRemainingItems(inv)
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
            'g':JC, new ItemStack(Blocks.STAINED_GLASS, 1, EnumColour.LIGHT_BLUE.getWoolDamage),
            'o':JC, Blocks.OBSIDIAN,
            'e':JC, "gemEmerald",
            'i':JC, "ingotIron",
            'w':JC, "plankWood"
        ))

        //IC Blueprint
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemICBlueprint),
            "pbp","brb","pbp",
            'p':JC, Items.PAPER,
            'b':JC, "dyeBlue",
            'r':JC, Items.REDSTONE
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

            override def getRemainingItems(inv:InventoryCrafting) = ForgeHooks.defaultRecipeGetRemainingItems(inv)
        })

        //IC Blueprint - copy
        GameRegistry.addRecipe(new IRecipe {
            override def matches(inv:InventoryCrafting, w:World) = getCraftingResult(inv) != null

            override def getRecipeOutput = new ItemStack(itemICBlueprint, 2)

            override def getRecipeSize = 9

            override def getCraftingResult(inv:InventoryCrafting):ItemStack =
            {
                var bp:ItemStack = null
                var emptyCount = 0
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
                            emptyCount += 1
                    }
                }
                if (bp != null && emptyCount > 0)
                {
                    val out = new ItemStack(itemICBlueprint)
                    out.stackSize = emptyCount+1
                    ItemICBlueprint.copyIC(bp, out)
                    out
                }
                else null
            }

            override def getRemainingItems(inv:InventoryCrafting) = ForgeHooks.defaultRecipeGetRemainingItems(inv)
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