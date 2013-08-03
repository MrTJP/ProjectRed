package mrtjp.projectred.integration;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.common.registry.GameRegistry;

public class IntegrationRecipes {

	public static void initIntegrationRecipes() {
		initGateRecipes();
		initMiscRecipes();
	}
	
	private static void initMiscRecipes() {
		/** Screw Driver **/
		GameRegistry.addRecipe(new ItemStack(ProjectRed.itemScrewdriver),
				"i  ",
				" ib",
				" bi",
				'i', Item.ingotIron,
				'b', new ItemStack(Item.dyePowder, 1, PRColors.BLUE.dyeId())
		);		
	}

	private static void initGateRecipes() {

		/** AND Gate **/
		GameRegistry.addRecipe(EnumGate.AND.getItemStack(), 
				"ACA",
				"CCC",
				"PWP",
				'A', EnumPart.ANODE.getItemStack(),  
				'C', EnumPart.CATHODE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** OR Gate **/
		GameRegistry.addRecipe(EnumGate.OR.getItemStack(), 
				"PCP",
				"WCW",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);

		/** NOT Gate**/
		GameRegistry.addRecipe(EnumGate.NOT.getItemStack(),
				"PCP",
				"CAC",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		
		/** RS Latch **/
		GameRegistry.addRecipe(EnumGate.RSLATCH.getItemStack(),
				"WWA",
				"CPC",
				"AWW",
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack()
		);

		/** Toggle Latch **/
		GameRegistry.addRecipe(EnumGate.TOGGLE.getItemStack(),
				"CPP",
				"WLW",
				"CPP",
				'C', EnumPart.CATHODE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'L', Block.lever
		);

		/** NOR Gate **/
		GameRegistry.addRecipe(EnumGate.NOR.getItemStack(),
				"PAP",
				"WCW",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack()
		);
		
		/** NAND Gate **/
		GameRegistry.addRecipe(EnumGate.NAND.getItemStack(),
				"AAA",
				"CCC",
				"PWP",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);

		/** XOR Gate **/
		GameRegistry.addRecipe(EnumGate.XOR.getItemStack(),
				"AWA",
				"CAC",
				"WCW",
				'A', EnumPart.ANODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack()
		);

		/** XNOR Gate **/
		GameRegistry.addRecipe(EnumGate.XNOR.getItemStack(),
				"ACA",
				"CAC",
				"WCW",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Buffer Gate **/
		GameRegistry.addRecipe(EnumGate.Buffer.getItemStack(),
				"ACA",
				"WCW",
				"PWP",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack()
		);
		
		/** Multiplexer Gate **/
		GameRegistry.addRecipe(EnumGate.Multiplexer.getItemStack(),
				"ACA",
				"CPC",
				"ACW",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Repeater Gate **/
		GameRegistry.addRecipe(EnumGate.Repeater.getItemStack(),
				"PCA",
				"ACP",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Timer Gate **/
		GameRegistry.addRecipe(EnumGate.Timer.getItemStack(),
				"ACA",
				"WTW",
				"PWP",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'T', EnumPart.POINTER.getItemStack(),
				'P', EnumPart.PLATE.getItemStack()
		);
		
		/** Counter Gate **/
		GameRegistry.addRecipe(EnumGate.Counter.getItemStack(),
				"PWP",
				"CTC",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'T', EnumPart.POINTER.getItemStack(),
				'A', EnumPart.ANODE.getItemStack()
		);
		
		/** Sequencer Gate **/
		GameRegistry.addRecipe(EnumGate.Sequencer.getItemStack(),
				"PCP",
				"CTC",
				"PCP",
				'P', EnumPart.PLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'T', EnumPart.POINTER.getItemStack()
		);
		
		/** Pulse Former Gate **/
		GameRegistry.addRecipe(EnumGate.PulseFormer.getItemStack(),
				"ACA",
				"CAC",
				"WWP",
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'P', EnumPart.PLATE.getItemStack()
		);
		
		/** Randomizer Gate **/
		GameRegistry.addRecipe(EnumGate.Randomizer.getItemStack(),
				"PEP",
				"WWW",
				"EWE",
				'P', EnumPart.PLATE.getItemStack(),
				'E', EnumPart.ENERGIZEDSILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** State Cell Gate **/
		GameRegistry.addRecipe(EnumGate.StateCell.getItemStack(),
				"PAC",
				"WST",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'T', EnumPart.POINTER.getItemStack()
		);
		
		/** Synchronizer Gate **/
		GameRegistry.addRecipe(EnumGate.Synchronizer.getItemStack(),
				"WCW",
				"SAS",
				"WWW",
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
				'C', EnumPart.CATHODE.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'A', EnumPart.ANODE.getItemStack()
		);
		
		/** D-Latch Gate **/
		GameRegistry.addRecipe(EnumGate.DLatch.getItemStack(),
				"PAP",
				"ASW",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** D-FlipFlop Gate **/
		GameRegistry.addRecipe(EnumGate.DFlop.getItemStack(),
				"PAP",
				"ASA",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'A', EnumPart.ANODE.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Bundled Latch **/
		GameRegistry.addRecipe(EnumGate.BundledLatch.getItemStack(),
				"PBP",
				"GSW",
				"PBP",
				'P', EnumPart.PLATE.getItemStack(),
				'B', EnumPart.BUNDLEDPLATE.getItemStack(),
				'G', EnumGate.DLatch.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Bundled Relay **/
		GameRegistry.addRecipe(EnumGate.BundledRelay.getItemStack(),
				"PBP",
				"GSW",
				"PBP",
				'P', EnumPart.PLATE.getItemStack(),
				'B', EnumPart.BUNDLEDPLATE.getItemStack(),
				'G', EnumGate.Repeater.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Bundled Multiplexer **/
		GameRegistry.addRecipe(EnumGate.BundledMultiplexer.getItemStack(),
				"PBP",
				"BSB",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'B', EnumPart.BUNDLEDPLATE.getItemStack(),
				'S', EnumPart.SILICONCHIP.getItemStack(),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
		
		/** Light Sensor **/
		GameRegistry.addRecipe(EnumGate.LightSensor.getItemStack(),
				"PPP",
				"LLL",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'L', new ItemStack(Item.dyePowder, 1, PRColors.BLUE.dyeId()),
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);

		/** Rain Sensor **/
		GameRegistry.addRecipe(EnumGate.RainSensor.getItemStack(),
				"PPP",
				"SSS",
				"PWP",
				'P', EnumPart.PLATE.getItemStack(),
				'S', Item.slimeBall,
				'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
		);
	}
}
