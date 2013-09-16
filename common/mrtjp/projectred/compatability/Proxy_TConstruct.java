package mrtjp.projectred.compatability;

import static tconstruct.common.TContent.metalPattern;
import mrtjp.projectred.compatability.Services.ITConstructProxy;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.block.Block;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import net.minecraft.block.material.MaterialLiquid;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraftforge.fluids.Fluid;
import net.minecraftforge.fluids.FluidRegistry;
import net.minecraftforge.fluids.FluidStack;
import tconstruct.TConstruct;
import tconstruct.common.TContent;
import tconstruct.library.TConstructRegistry;
import tconstruct.library.crafting.Smeltery;
import cpw.mods.fml.common.registry.GameRegistry;

public class Proxy_TConstruct implements ITConstructProxy {
	
	public static Material liquidMetal;
	
	public static Fluid moltenRedstoneFluid;
	public static LiquidFiniteSubstance moltenRedstone;
	
	public static Fluid moltenConductiveRedmetalFluid;
	public static LiquidFiniteSubstance moltenConductiveRedmetal;

	public Proxy_TConstruct() {
		System.out.println("[ProjectRedCompat] Loaded TConstruct Proxy");
	}
	
	@Override
	public void addSmeltingRecipe(ItemStack input, int blockID, int metadata, int temperature, FluidStack liquid) {
		Smeltery.addMelting(input, blockID, metadata, temperature, liquid);
	}

	@Override
	public void addAlloyMixing(FluidStack result, FluidStack... mixers) {
		Smeltery.addAlloyMixing(result, mixers);
	}
	
	@Override
    public void addTableCastingRecipe(ItemStack output, FluidStack metal, ItemStack cast, boolean consume, int delay) {
		TConstructRegistry.instance.getTableCasting().addCastingRecipe(output, metal, cast, consume, delay);
	}
	
	@Override
	public void addBasinCastingRecipe(ItemStack output, FluidStack metal, ItemStack cast, boolean consume, int delay) {
		TConstructRegistry.instance.getBasinCasting().addCastingRecipe(output, metal, cast, consume, delay);
	}
	
	@Override
	public ItemStack getCastIngot() {
		return new ItemStack(metalPattern, 1, 0);
	}

    
	@Override
	public void loadTCInteractions() {
		 liquidMetal = new MaterialLiquid(MapColor.tntColor);
		 
		 moltenRedstoneFluid = new Fluid("Molten Redstone");
		 FluidRegistry.registerFluid(moltenRedstoneFluid);
		 moltenRedstone = (LiquidFiniteSubstance) new LiquidFiniteSubstance(Configurator.block_redstoneliquidID.getInt(), moltenRedstoneFluid, "liqredstone", liquidMetal).setUnlocalizedName("projectred.compatability.liqredstone");
		 GameRegistry.registerBlock(moltenRedstone, "projectred.compatability.liqredstone");
		 moltenRedstoneFluid.setBlockID(moltenRedstone).setLuminosity(12).setDensity(1000).setViscosity(3000);
		 
		 moltenConductiveRedmetalFluid = new Fluid("Molten Conductive Redmetal");
		 FluidRegistry.registerFluid(moltenConductiveRedmetalFluid);
		 moltenConductiveRedmetal = (LiquidFiniteSubstance) new LiquidFiniteSubstance(Configurator.block_redconductiveliquidID.getInt(), moltenConductiveRedmetalFluid, "liqcondredmetal", liquidMetal).setUnlocalizedName("projectred.compatability.liqcondredmetal");
		 GameRegistry.registerBlock(moltenConductiveRedmetal, "projectred.compatability.liqcondredmetal");
		 moltenConductiveRedmetalFluid.setBlockID(moltenConductiveRedmetal).setLuminosity(12).setDensity(2000).setViscosity(4000);

		 // Molten redstone recipes
		 addSmeltingRecipe(new ItemStack(Item.redstone, 4), Block.blockRedstone.blockID, 0, 160, new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue));
		 addSmeltingRecipe(new ItemStack(Block.blockRedstone), Block.blockRedstone.blockID, 0, 575, new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue*9));
		 
		 // Conductive Redmetal recipes
		 addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue*4), new FluidStack(TContent.moltenIronFluid, TConstruct.ingotLiquidValue*1));
		 addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue*3), new FluidStack(TContent.moltenCopperFluid, TConstruct.ingotLiquidValue*2));
		 addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue*2), new FluidStack(TContent.moltenTinFluid, TConstruct.ingotLiquidValue*3));
		 addAlloyMixing(new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), new FluidStack(moltenRedstoneFluid, TConstruct.ingotLiquidValue*1), new FluidStack(TContent.moltenAluminumFluid, TConstruct.ingotLiquidValue*4));
		 
		 // Red alloy ingot casting
		 addTableCastingRecipe(EnumPart.REDINGOT.getItemStack(), new FluidStack(moltenConductiveRedmetalFluid, TConstruct.ingotLiquidValue), getCastIngot(), false, 32);
	}
}
