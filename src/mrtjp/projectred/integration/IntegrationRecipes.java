package mrtjp.projectred.integration;

import cpw.mods.fml.common.registry.GameRegistry;
import mrtjp.projectred.core.PartDefs$;
import mrtjp.projectred.core.libmc.PRColors;
import net.minecraft.init.Blocks;
import net.minecraft.init.Items;
import net.minecraft.item.ItemStack;

public class IntegrationRecipes
{
    public static void initRecipes()
    {
        initGateRecipes();
    }

    private static void initGateRecipes()
    {
        /** AND Gate **/
        GameRegistry.addRecipe(EnumGate.AND.makeStack(),
                "ACA",
                "CCC",
                "PWP",
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack()
                );

        /** OR Gate **/
        GameRegistry.addRecipe(EnumGate.OR.makeStack(),
                "PCP",
                "WCW",
                "PWP",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack()
                );

        /** NOT Gate**/
        GameRegistry.addRecipe(EnumGate.NOT.makeStack(),
                "PCP",
                "CAC",
                "PWP",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack()
                );


        /** RS Latch **/
        GameRegistry.addRecipe(EnumGate.RSLatch.makeStack(),
                "ACW",
                "WPW",
                "WCA",
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'P', PartDefs$.MODULE$.PLATE().makeStack()
                );

        /** Toggle Latch **/
        GameRegistry.addRecipe(EnumGate.ToggleLatch.makeStack(),
                "CPP",
                "WLW",
                "CPP",
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'L', Blocks.lever
                );

        /** Transparent Latch **/
        GameRegistry.addRecipe(EnumGate.TransparentLatch.makeStack(),
                "ACW",
                "CCC",
                "CWP",
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack()
                );

        /** NOR Gate **/
        GameRegistry.addRecipe(EnumGate.NOR.makeStack(),
                "PAP",
                "WCW",
                "PWP",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack()
                );

        /** NAND Gate **/
        GameRegistry.addRecipe(EnumGate.NAND.makeStack(),
                "AAA",
                "CCC",
                "PWP",
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack()
                );

        /** XOR Gate **/
        GameRegistry.addRecipe(EnumGate.XOR.makeStack(),
                "AWA",
                "CAC",
                "WCW",
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack()
                );

        /** XNOR Gate **/
        GameRegistry.addRecipe(EnumGate.XNOR.makeStack(),
                "ACA",
                "CAC",
                "WCW",
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack()
                );

        /** Buffer Gate **/
        GameRegistry.addRecipe(EnumGate.Buffer.makeStack(),
                "ACA",
                "WCW",
                "PWP",
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'P', PartDefs$.MODULE$.PLATE().makeStack()
                );

        /** Multiplexer Gate **/
        GameRegistry.addRecipe(EnumGate.Multiplexer.makeStack(),
                "ACA",
                "CPC",
                "ACW",
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack()
                );

        /** Repeater Gate **/
        GameRegistry.addRecipe(EnumGate.Repeater.makeStack(),
                "PCA",
                "ACP",
                "PWP",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack()
                );

        /** Timer Gate **/
        GameRegistry.addRecipe(EnumGate.Timer.makeStack(),
                "ACA",
                "WTW",
                "PWP",
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'T', PartDefs$.MODULE$.POINTER().makeStack(),
                'P', PartDefs$.MODULE$.PLATE().makeStack()
                );

        /** Counter Gate **/
        GameRegistry.addRecipe(EnumGate.Counter.makeStack(),
                "PCP",
                "WWT",
                "PCP",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'T', PartDefs$.MODULE$.POINTER().makeStack(),
                'A', PartDefs$.MODULE$.ANODE().makeStack()
                );

        /** Sequencer Gate **/
        GameRegistry.addRecipe(EnumGate.Sequencer.makeStack(),
                "PCP",
                "CTC",
                "PCP",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'T', PartDefs$.MODULE$.POINTER().makeStack()
                );

        /** Pulse Former Gate **/
        GameRegistry.addRecipe(EnumGate.Pulse.makeStack(),
                "ACA",
                "CAC",
                "WWP",
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'P', PartDefs$.MODULE$.PLATE().makeStack()
                );

        /** Randomizer Gate **/
        GameRegistry.addRecipe(EnumGate.Randomizer.makeStack(),
                "PEP",
                "WWW",
                "EWE",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'E', PartDefs$.MODULE$.ENERGIZEDSILICONCHIP().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack()
                );

        /** State Cell Gate **/
        GameRegistry.addRecipe(EnumGate.StateCell.makeStack(),
                "PAC",
                "WST",
                "PWP",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'A', PartDefs$.MODULE$.ANODE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'S', PartDefs$.MODULE$.SILICONCHIP().makeStack(),
                'T', PartDefs$.MODULE$.POINTER().makeStack()
                );

        /** Synchronizer Gate **/
        GameRegistry.addRecipe(EnumGate.Synchronizer.makeStack(),
                "WCW",
                "SAS",
                "WWW",
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'S', PartDefs$.MODULE$.SILICONCHIP().makeStack(),
                'A', PartDefs$.MODULE$.ANODE().makeStack()
                );

        /** Light Sensor **/
        GameRegistry.addRecipe(EnumGate.LightSensor.makeStack(),
                "PPP",
                "LLL",
                "PWP",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'L', new ItemStack(Items.dye, 1, PRColors.BLUE.dyeId()),
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack()
                );

        /** Rain Sensor **/
        GameRegistry.addRecipe(EnumGate.RainSensor.makeStack(),
                "PPP",
                "SSS",
                "PWP",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'S', Items.slime_ball,
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack()
                );

        /** Bundled Latch **/
        GameRegistry.addRecipe(EnumGate.BusTransceiver.makeStack(),
                "BBB",
                "SPS",
                "BBB",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'B', PartDefs$.MODULE$.BUNDLEDPLATE().makeStack(),
                'S', PartDefs$.MODULE$.SILICONCHIP().makeStack()
                );

        /** Null Cell **/
        GameRegistry.addRecipe(EnumGate.NullCell.makeStack(),
                "PWP",
                "WSW",
                "PWP",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'W', PartDefs$.MODULE$.WIREDPLATE().makeStack(),
                'S', PartDefs$.MODULE$.PLATFORMEDPLATE().makeStack()
                );

        /** Invert Cell **/
        GameRegistry.addRecipe(EnumGate.InvertCell.makeStack(),
                "PWP",
                "WSW",
                "PCP",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'W', PartDefs$.MODULE$.WIREDPLATE().makeStack(),
                'S', PartDefs$.MODULE$.PLATFORMEDPLATE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack()
                );

        /** Buffer Cell **/
        GameRegistry.addRecipe(EnumGate.BufferCell.makeStack(),
                "PWP",
                "WSW",
                "PCC",
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'W', PartDefs$.MODULE$.WIREDPLATE().makeStack(),
                'S', PartDefs$.MODULE$.PLATFORMEDPLATE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack()
                );

        /** Comparator **/
        GameRegistry.addRecipe(EnumGate.Comparator.makeStack(),
                "WCW",
                "QWQ",
                "PWP",
                'W', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack(),
                'Q', Items.quartz,
                'P', PartDefs$.MODULE$.PLATE().makeStack()
                );

        /** AND Cell **/
        GameRegistry.addRecipe(EnumGate.ANDCell.makeStack(),
                "CwC",
                "WSW",
                "PwC",
                'w', PartDefs$.MODULE$.CONDUCTIVEPLATE().makeStack(),
                'W', PartDefs$.MODULE$.WIREDPLATE().makeStack(),
                'S', PartDefs$.MODULE$.PLATFORMEDPLATE().makeStack(),
                'P', PartDefs$.MODULE$.PLATE().makeStack(),
                'C', PartDefs$.MODULE$.CATHODE().makeStack()
                );
    }
}
