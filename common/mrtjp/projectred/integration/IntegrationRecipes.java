package mrtjp.projectred.integration;

import mrtjp.projectred.ProjectRedIntegration;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.core.PRColors;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.common.registry.GameRegistry;

public class IntegrationRecipes {

    public static void initIntegrationRecipes() {
        initGateRecipes();
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
        GameRegistry.addRecipe(EnumGate.RSLatch.getItemStack(),
                "ACW",
                "WPW",
                "WCA",
                'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
                'A', EnumPart.ANODE.getItemStack(),
                'C', EnumPart.CATHODE.getItemStack(),
                'P', EnumPart.PLATE.getItemStack()
        );

        /** Toggle Latch **/
        GameRegistry.addRecipe(EnumGate.ToggleLatch.getItemStack(),
                "CPP",
                "WLW",
                "CPP",
                'C', EnumPart.CATHODE.getItemStack(),
                'P', EnumPart.PLATE.getItemStack(),
                'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
                'L', Block.lever
        );
        
        /** Transparent Latch **/
        GameRegistry.addRecipe(EnumGate.TransparentLatch.getItemStack(),
                "ACW",
                "CCC",
                "CWP",
                'C', EnumPart.CATHODE.getItemStack(),
                'A', EnumPart.ANODE.getItemStack(),
                'P', EnumPart.PLATE.getItemStack(),
                'W', EnumPart.CONDUCTIVEPLATE.getItemStack()
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
                "PCP",
                "WWT",
                "PCP",
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
        GameRegistry.addRecipe(EnumGate.Pulse.getItemStack(),
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
        
        /** Bundled Latch **/
        GameRegistry.addRecipe(EnumGate.BusTransceiver.getItemStack(),
                "BBB",
                "SPS",
                "BBB",
                'P', EnumPart.PLATE.getItemStack(),
                'B', EnumPart.BUNDLEDPLATE.getItemStack(),
                'S', EnumPart.SILICONCHIP.getItemStack()
        );
        
        /** Null Cell **/
        GameRegistry.addRecipe(EnumGate.NullCell.getItemStack(),
                "PWP",
                "WSW",
                "PWP",
                'P', EnumPart.PLATE.getItemStack(),
                'W', EnumPart.WIREDPLATE.getItemStack(),
                'S', EnumPart.PLATFORMEDPLATE.getItemStack()
        );
        
        /** Invert Cell **/
        GameRegistry.addRecipe(EnumGate.InvertCell.getItemStack(),
                "PWP",
                "WSW",
                "PCP",
                'P', EnumPart.PLATE.getItemStack(),
                'W', EnumPart.WIREDPLATE.getItemStack(),
                'S', EnumPart.PLATFORMEDPLATE.getItemStack(),
                'C', EnumPart.CATHODE.getItemStack()
        );

        /** Buffer Cell **/
        GameRegistry.addRecipe(EnumGate.BufferCell.getItemStack(),
                "PWP",
                "WSW",
                "PCC",
                'P', EnumPart.PLATE.getItemStack(),
                'W', EnumPart.WIREDPLATE.getItemStack(),
                'S', EnumPart.PLATFORMEDPLATE.getItemStack(),
                'C', EnumPart.CATHODE.getItemStack()
        );
        
        /** Comparator **/
        GameRegistry.addRecipe(EnumGate.Comparator.getItemStack(),
        		"WCW",
        		"QWQ",
        		"PWP",
        		'W', EnumPart.CONDUCTIVEPLATE.getItemStack(),
        		'C', EnumPart.CATHODE.getItemStack(),
        		'Q', Item.netherQuartz,
        		'P', EnumPart.PLATE.getItemStack()
        );
        
        /** AND Cell **/
        GameRegistry.addRecipe(EnumGate.ANDCell.getItemStack(),
        		"CwC",
        		"WSW",
        		"PwC",
        		'w', EnumPart.CONDUCTIVEPLATE.getItemStack(),
        		'W', EnumPart.WIREDPLATE.getItemStack(),
        		'S', EnumPart.PLATFORMEDPLATE.getItemStack(),
        		'P', EnumPart.PLATE.getItemStack(),
        		'C', EnumPart.CATHODE.getItemStack()
       );
        
    }
}
