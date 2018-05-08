/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication
import java.lang.{Character => JC}

import codechicken.lib.model.ModelRegistryHelper
import codechicken.lib.model.bakery.CCBakeryModel
import codechicken.lib.model.bakery.key.IBlockStateKeyGenerator
import codechicken.lib.render.item.map.MapRenderRegistry
import codechicken.lib.texture.TextureUtils
import codechicken.lib.texture.TextureUtils.IIconRegister
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.api.IPartFactory
import mrtjp.core.block.{ItemBlockCore, MultiTileBlock}
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedFabrication._
import mrtjp.projectred.ProjectRedIntegration
import mrtjp.projectred.core.IProxy
import mrtjp.projectred.integration.{GateDefinition, RenderGate}
import net.minecraft.block.Block
import net.minecraft.client.renderer.block.model.{ModelResourceLocation, ModelBakery => MCModelBakery}
import net.minecraft.client.renderer.block.statemap.IStateMapper
import net.minecraft.client.renderer.block.statemap.StateMap.Builder
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.ResourceLocation
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.client.registry.ClientRegistry
import net.minecraftforge.fml.common.registry.ForgeRegistries
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class FabricationProxy_server extends IProxy with IPartFactory
{
    override def preinit()
    {
        icBlock = new BlockICMachine(icMachineBakery)
        icBlock.setUnlocalizedName("projectred.fabrication.icMachine")
        ForgeRegistries.BLOCKS.register(icBlock.setRegistryName("ic_machine"))
        ForgeRegistries.ITEMS.register(new ItemBlockCore(icBlock).setRegistryName(icBlock.getRegistryName))
        icBlock.addTile(classOf[TileICWorkbench], 0)
        icBlock.addTile(classOf[TileICPrinter], 1)

        itemICBlueprint = new ItemICBlueprint
        itemICBlueprint.setUnlocalizedName("projectred.fabrication.icBlueprint")
        ForgeRegistries.ITEMS.register(itemICBlueprint.setRegistryName("ic_blueprint"))

        itemICChip = new ItemICChip
        itemICChip.setUnlocalizedName("projectred.fabrication.icChip")
        ForgeRegistries.ITEMS.register(itemICChip.setRegistryName("ic_chip"))

        MultiPartRegistry.registerParts(this, Array(GateDefinition.typeICGate))
        MinecraftForge.EVENT_BUS.register(FabricationRecipes)
    }

    override def init(){}

    override def postinit()
    {
        //hook into gate part to add tooltip info
        ProjectRedIntegration.itemPartGate.infoBuilderFunc = {(stack, list) =>
            if (stack.getItemDamage == GateDefinition.ICGate.meta) {
                import com.mojang.realmsclient.gui.ChatFormatting._
                if (!ItemICBlueprint.hasICInside(stack)) {
                    list.add(RED + "INVALID: Craft by surrounding Printed IC with Circuit Plates")
                } else {
                    list.add(GRAY+ItemICBlueprint.getICName(stack))
                }
            }
        }
    }

    /**
      * Create a new instance of the part with the specified type name identifier
      *
      * @param client If the part instance is for the client or the server
      */
    override def createPart(name:ResourceLocation, client:Boolean) = name match
    {
        case GateDefinition.typeICGate => new ICGatePart
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

        {
            val model = new CCBakeryModel()
            val regLoc = icBlock.getRegistryName
            val normalLoc = new ModelResourceLocation(regLoc, "normal")
            val wrappedLoc = new ModelResourceLocation(regLoc, "printer_wrapped")
            ModelLoader.setCustomStateMapper(icBlock, new Builder().ignore(MultiTileBlock.TILE_INDEX).build())
            ModelLoader.setCustomMeshDefinition(Item.getItemFromBlock(icBlock), stack => if(stack.getMetadata == 1) wrappedLoc else normalLoc)
            ModelRegistryHelper.register(normalLoc, model)
            ModelRegistryHelper.register(wrappedLoc, RenderICPrinterItem)
            TextureUtils.addIconRegister(icMachineBakery.registerKeyGens(icBlock))
        }

        registerModelType(itemICBlueprint, "projectred:fabrication/items", "ic_blueprint")
        MapRenderRegistry.registerMapRenderer(itemICBlueprint, ItemRenderICBlueprint)

        registerModelType(itemICChip, "projectred:fabrication/items", Array("ic_active", "ic_inert"), { stack =>
            if (ItemICBlueprint.hasICInside(stack)) "ic_active" else "ic_inert" })

        ClientRegistry.bindTileEntitySpecialRenderer(classOf[TileICPrinter], RenderICPrinterDynamic)

        GuiHandler.register(GuiICWorkbench, icWorkbenchGui)
        GuiHandler.register(GuiICPrinter, icPrinterGui)

        RenderGate.hotswap(new RenderICGate, GateDefinition.ICGate.ordinal)
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
    def registerModelType(item:Item, jsonLocation:String, names:Array[String], typeValue:ItemStack => String)
    {
        MCModelBakery.registerItemVariants(item, names.map { n => new ModelResourceLocation(jsonLocation, s"type=$n") }:_*)
        ModelLoader.setCustomMeshDefinition(item, (s: ItemStack) => new ModelResourceLocation(jsonLocation, "type=" + typeValue(s)))
    }
}

object FabricationProxy extends FabricationProxy_client
