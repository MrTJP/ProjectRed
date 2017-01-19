package mrtjp.projectred.compatibility.tconstruct

import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.core.{Configurator, PartDefs}
import net.minecraft.block.Block
import net.minecraft.block.material.{MapColor, Material, MaterialLiquid}
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack
import net.minecraftforge.fluids.{Fluid, FluidRegistry, FluidStack}
import tconstruct.TConstruct
import tconstruct.library.TConstructRegistry
import tconstruct.library.crafting.Smeltery
import tconstruct.smeltery.TinkerSmeltery._

object PluginTConstruct extends IPRPlugin
{
    var liquidMetal:Material = null
    var moltenRedstoneFluid:Fluid = null
    var moltenRedstone:LiquidFiniteSubstance = null
    var moltenConductiveRedmetalFluid:Fluid = null
    var moltenConductiveRedmetal:LiquidFiniteSubstance = null

    override def getModIDs = Array("TConstruct")

    override def isEnabled = Configurator.compat_TConstruct

    def addSmeltingRecipe(input:ItemStack, block:Block, metadata:Int, temperature:Int, liquid:FluidStack)
    {
        Smeltery.addMelting(input, block, metadata, temperature, liquid)
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

        moltenRedstoneFluid = new Fluid("redstone.molten")
        FluidRegistry.registerFluid(moltenRedstoneFluid)
        moltenRedstone = new LiquidFiniteSubstance(moltenRedstoneFluid, "liquid_redstone", liquidMetal).setBlockName("projectred.compatibility.liqredstone").asInstanceOf[LiquidFiniteSubstance]
        GameRegistry.registerBlock(moltenRedstone, "projectred.compatibility.liqredstone")
        moltenRedstoneFluid.setBlock(moltenRedstone).setLuminosity(12).setDensity(1000).setViscosity(3000)

        moltenConductiveRedmetalFluid = new Fluid("redmetal.molten")
        FluidRegistry.registerFluid(moltenConductiveRedmetalFluid)
        moltenConductiveRedmetal = new LiquidFiniteSubstance(moltenConductiveRedmetalFluid, "liquid_redmetal", liquidMetal).setBlockName("projectred.compatibility.liqcondredmetal").asInstanceOf[LiquidFiniteSubstance]
        GameRegistry.registerBlock(moltenConductiveRedmetal, "projectred.compatibility.liqcondredmetal")
        moltenConductiveRedmetalFluid.setBlock(moltenConductiveRedmetal).setLuminosity(12).setDensity(2000).setViscosity(4000)

        // Molten redstone recipes
        addSmeltingRecipe(new ItemStack(Items.redstone, 4), Blocks.redstone_block, 0, 160, new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue))
        addSmeltingRecipe(new ItemStack(Blocks.redstone_block), Blocks.redstone_block, 0, 575, new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue * 9))

        // Conductive Redmetal recipes
        addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue * 4), new FluidStack(moltenIronFluid, TConstruct.ingotLiquidValue * 1))
        addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue * 3), new FluidStack(moltenCopperFluid, TConstruct.ingotLiquidValue * 2))
        addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue * 2), new FluidStack(moltenTinFluid, TConstruct.ingotLiquidValue * 3))
        addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue * 1), new FluidStack(moltenAluminumFluid, TConstruct.ingotLiquidValue * 4))

        // Red alloy ingot casting
        addTableCastingRecipe(PartDefs.REDINGOT.makeStack, new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), getCastIngot, false, 32)

        // Resmelting
        addSmeltingRecipe(PartDefs.REDINGOT.makeStack, Blocks.redstone_block, 0, 550+160, new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue))
    }

    override def postInit() {}

    override def desc() = "Tinker's Construct: Smeltery recipes"
}
