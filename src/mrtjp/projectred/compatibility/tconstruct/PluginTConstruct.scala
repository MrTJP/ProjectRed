package mrtjp.projectred.compatibility.tconstruct

import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.core.ItemPart.EnumPart
import net.minecraft.block.Block
import net.minecraft.block.material.{MapColor, MaterialLiquid, Material}
import net.minecraft.item.{Item, ItemStack}
import net.minecraftforge.fluids.{FluidStack, FluidRegistry, Fluid}
import tconstruct.TConstruct
import tconstruct.common.TContent
import tconstruct.common.TContent._
import tconstruct.library.TConstructRegistry
import tconstruct.library.crafting.Smeltery
import mrtjp.projectred.compatibility.IPRPlugin

class PluginTConstruct extends IPRPlugin
{
    var liquidMetal:Material = null
    var moltenRedstoneFluid:Fluid = null
    var moltenRedstone:LiquidFiniteSubstance = null
    var moltenConductiveRedmetalFluid:Fluid = null
    var moltenConductiveRedmetal:LiquidFiniteSubstance = null

    override def getModID = "TConstruct"

    def addSmeltingRecipe(input:ItemStack, blockID:Int, metadata:Int, temperature:Int, liquid:FluidStack)
    {
        Smeltery.addMelting(input, blockID, metadata, temperature, liquid)
    }

    def addAlloyMixing(result:FluidStack, mixers:FluidStack*)
    {
        Smeltery.addAlloyMixing(result, mixers:_*)
    }

    def addTableCastingRecipe(output:ItemStack, metal:FluidStack, cast:ItemStack, consume:Boolean, delay:Int)
    {
        TConstructRegistry.getTableCasting.addCastingRecipe(output, metal, cast, consume, delay)
    }

    def addBasinCastingRecipe(output:ItemStack, metal:FluidStack, cast:ItemStack, consume:Boolean, delay:Int)
    {
        TConstructRegistry.getBasinCasting.addCastingRecipe(output, metal, cast, consume, delay)
    }

    def getCastIngot = new ItemStack(metalPattern, 1, 0)


    override def preInit() {}

    override def init()
    {
        liquidMetal = new MaterialLiquid(MapColor.tntColor)

        moltenRedstoneFluid = new Fluid("Molten Redstone")
        FluidRegistry.registerFluid(moltenRedstoneFluid)
        moltenRedstone = new LiquidFiniteSubstance(Configurator.block_redstoneliquidID.getInt, moltenRedstoneFluid, "liqredstone", liquidMetal).setUnlocalizedName("projectred.compatibility.liqredstone").asInstanceOf[LiquidFiniteSubstance]
        GameRegistry.registerBlock(moltenRedstone, "projectred.compatibility.liqredstone")
        moltenRedstoneFluid.setBlockID(moltenRedstone).setLuminosity(12).setDensity(1000).setViscosity(3000)

        moltenConductiveRedmetalFluid = new Fluid("Molten Conductive Redmetal")
        FluidRegistry.registerFluid(moltenConductiveRedmetalFluid)
        moltenConductiveRedmetal = new LiquidFiniteSubstance(Configurator.block_redconductiveliquidID.getInt, moltenConductiveRedmetalFluid, "liqcondredmetal", liquidMetal).setUnlocalizedName("projectred.compatibility.liqcondredmetal").asInstanceOf[LiquidFiniteSubstance]
        GameRegistry.registerBlock(moltenConductiveRedmetal, "projectred.compatibility.liqcondredmetal")
        moltenConductiveRedmetalFluid.setBlockID(moltenConductiveRedmetal).setLuminosity(12).setDensity(2000).setViscosity(4000)

        // Molten redstone recipes
        addSmeltingRecipe(new ItemStack(Item.redstone, 4), Block.blockRedstone.blockID, 0, 160, new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue))
        addSmeltingRecipe(new ItemStack(Block.blockRedstone), Block.blockRedstone.blockID, 0, 575, new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue * 9))

        // Conductive Redmetal recipes
        addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue * 4), new FluidStack(TContent.moltenIronFluid, TConstruct.ingotLiquidValue * 1))
        addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue * 3), new FluidStack(TContent.moltenCopperFluid, TConstruct.ingotLiquidValue * 2))
        addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue * 2), new FluidStack(TContent.moltenTinFluid, TConstruct.ingotLiquidValue * 3))
        addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue * 1), new FluidStack(TContent.moltenAluminumFluid, TConstruct.ingotLiquidValue * 4))

        // Red alloy ingot casting
        addTableCastingRecipe(EnumPart.REDINGOT.getItemStack, new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), getCastIngot, false, 32)
    }

    override def postInit() {}
}
