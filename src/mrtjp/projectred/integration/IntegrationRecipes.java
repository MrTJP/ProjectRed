package mrtjp.projectred.integration;

import cpw.mods.fml.common.registry.GameRegistry;
import mrtjp.projectred.core.PartDefs;
import mrtjp.core.color.Colors;
import net.minecraft.init.Blocks;
import net.minecraft.init.Items;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.ShapedOreRecipe;

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
                'A', PartDefs.ANODE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'P', PartDefs.PLATE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack()
                );

        /** OR Gate **/
        GameRegistry.addRecipe(EnumGate.OR.makeStack(),
                "PCP",
                "WCW",
                "PWP",
                'P', PartDefs.PLATE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack()
                );

        /** NOT Gate**/
        GameRegistry.addRecipe(EnumGate.NOT.makeStack(),
                "PCP",
                "CAC",
                "PWP",
                'P', PartDefs.PLATE().makeStack(),
                'A', PartDefs.ANODE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack()
                );


        /** RS Latch **/
        GameRegistry.addRecipe(EnumGate.RSLatch.makeStack(),
                "ACW",
                "WPW",
                "WCA",
                'W', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'A', PartDefs.ANODE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'P', PartDefs.PLATE().makeStack()
                );

        /** Toggle Latch **/
        GameRegistry.addRecipe(EnumGate.ToggleLatch.makeStack(),
                "CPP",
                "WLW",
                "CPP",
                'C', PartDefs.CATHODE().makeStack(),
                'P', PartDefs.PLATE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'L', Blocks.lever
                );

        /** Transparent Latch **/
        GameRegistry.addRecipe(EnumGate.TransparentLatch.makeStack(),
                "ACW",
                "CCC",
                "CWP",
                'C', PartDefs.CATHODE().makeStack(),
                'A', PartDefs.ANODE().makeStack(),
                'P', PartDefs.PLATE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack()
                );

        /** NOR Gate **/
        GameRegistry.addRecipe(EnumGate.NOR.makeStack(),
                "PAP",
                "WCW",
                "PWP",
                'P', PartDefs.PLATE().makeStack(),
                'A', PartDefs.ANODE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'C', PartDefs.CATHODE().makeStack()
                );

        /** NAND Gate **/
        GameRegistry.addRecipe(EnumGate.NAND.makeStack(),
                "AAA",
                "CCC",
                "PWP",
                'A', PartDefs.ANODE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'P', PartDefs.PLATE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack()
                );

        /** XOR Gate **/
        GameRegistry.addRecipe(EnumGate.XOR.makeStack(),
                "AWA",
                "CAC",
                "WCW",
                'A', PartDefs.ANODE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'C', PartDefs.CATHODE().makeStack()
                );

        /** XNOR Gate **/
        GameRegistry.addRecipe(EnumGate.XNOR.makeStack(),
                "ACA",
                "CAC",
                "WCW",
                'A', PartDefs.ANODE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack()
                );

        /** Buffer Gate **/
        GameRegistry.addRecipe(EnumGate.Buffer.makeStack(),
                "ACA",
                "WCW",
                "PWP",
                'A', PartDefs.ANODE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'P', PartDefs.PLATE().makeStack()
                );

        /** Multiplexer Gate **/
        GameRegistry.addRecipe(EnumGate.Multiplexer.makeStack(),
                "ACA",
                "CPC",
                "ACW",
                'A', PartDefs.ANODE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'P', PartDefs.PLATE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack()
                );

        /** Repeater Gate **/
        GameRegistry.addRecipe(EnumGate.Repeater.makeStack(),
                "PCA",
                "ACP",
                "PWP",
                'P', PartDefs.PLATE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'A', PartDefs.ANODE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack()
                );

        /** Timer Gate **/
        GameRegistry.addRecipe(EnumGate.Timer.makeStack(),
                "ACA",
                "WTW",
                "PWP",
                'A', PartDefs.ANODE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'T', PartDefs.POINTER().makeStack(),
                'P', PartDefs.PLATE().makeStack()
                );

        /** Counter Gate **/
        GameRegistry.addRecipe(EnumGate.Counter.makeStack(),
                "PCP",
                "WWT",
                "PCP",
                'P', PartDefs.PLATE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'T', PartDefs.POINTER().makeStack(),
                'A', PartDefs.ANODE().makeStack()
                );

        /** Sequencer Gate **/
        GameRegistry.addRecipe(EnumGate.Sequencer.makeStack(),
                "PCP",
                "CTC",
                "PCP",
                'P', PartDefs.PLATE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'T', PartDefs.POINTER().makeStack()
                );

        /** Pulse Former Gate **/
        GameRegistry.addRecipe(EnumGate.Pulse.makeStack(),
                "ACA",
                "CAC",
                "WWP",
                'A', PartDefs.ANODE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'P', PartDefs.PLATE().makeStack()
                );

        /** Randomizer Gate **/
        GameRegistry.addRecipe(EnumGate.Randomizer.makeStack(),
                "PEP",
                "WWW",
                "EWE",
                'P', PartDefs.PLATE().makeStack(),
                'E', PartDefs.ENERGIZEDSILICONCHIP().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack()
                );

        /** State Cell Gate **/
        GameRegistry.addRecipe(EnumGate.StateCell.makeStack(),
                "PAC",
                "WST",
                "PWP",
                'P', PartDefs.PLATE().makeStack(),
                'A', PartDefs.ANODE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'W', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'S', PartDefs.SILICONCHIP().makeStack(),
                'T', PartDefs.POINTER().makeStack()
                );

        /** Synchronizer Gate **/
        GameRegistry.addRecipe(EnumGate.Synchronizer.makeStack(),
                "WCW",
                "SAS",
                "WWW",
                'W', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'S', PartDefs.SILICONCHIP().makeStack(),
                'A', PartDefs.ANODE().makeStack()
                );

        /** Light Sensor **/
        GameRegistry.addRecipe(EnumGate.LightSensor.makeStack(),
                "PPP",
                "LLL",
                "PWP",
                'P', PartDefs.PLATE().makeStack(),
                'L', "dyeBlue",
                'W', PartDefs.CONDUCTIVEPLATE().makeStack()
                );

        /** Rain Sensor **/
        GameRegistry.addRecipe(EnumGate.RainSensor.makeStack(),
                "PPP",
                "SSS",
                "PWP",
                'P', PartDefs.PLATE().makeStack(),
                'S', "slimeball",
                'W', PartDefs.CONDUCTIVEPLATE().makeStack()
                );

        /** Bus Transceiver **/
        GameRegistry.addRecipe(EnumGate.BusTransceiver.makeStack(),
                "BBB",
                "SPS",
                "BBB",
                'P', PartDefs.PLATE().makeStack(),
                'B', PartDefs.BUNDLEDPLATE().makeStack(),
                'S', PartDefs.SILICONCHIP().makeStack()
                );

        /** Null Cell **/
        GameRegistry.addRecipe(EnumGate.NullCell.makeStack(),
                "PWP",
                "WSW",
                "PWP",
                'P', PartDefs.PLATE().makeStack(),
                'W', PartDefs.WIREDPLATE().makeStack(),
                'S', PartDefs.PLATFORMEDPLATE().makeStack()
                );

        /** Invert Cell **/
        GameRegistry.addRecipe(EnumGate.InvertCell.makeStack(),
                "PWP",
                "WSW",
                "PCP",
                'P', PartDefs.PLATE().makeStack(),
                'W', PartDefs.WIREDPLATE().makeStack(),
                'S', PartDefs.PLATFORMEDPLATE().makeStack(),
                'C', PartDefs.CATHODE().makeStack()
                );

        /** Buffer Cell **/
        GameRegistry.addRecipe(EnumGate.BufferCell.makeStack(),
                "PWP",
                "WSW",
                "PCC",
                'P', PartDefs.PLATE().makeStack(),
                'W', PartDefs.WIREDPLATE().makeStack(),
                'S', PartDefs.PLATFORMEDPLATE().makeStack(),
                'C', PartDefs.CATHODE().makeStack()
                );

        /** Comparator **/
        GameRegistry.addRecipe(EnumGate.Comparator.makeStack(),
                "WCW",
                "QWQ",
                "PWP",
                'W', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'C', PartDefs.CATHODE().makeStack(),
                'Q', "gemQuartz",
                'P', PartDefs.PLATE().makeStack()
                );

        /** AND Cell **/
        GameRegistry.addRecipe(EnumGate.ANDCell.makeStack(),
                "CwC",
                "WSW",
                "PwC",
                'w', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'W', PartDefs.WIREDPLATE().makeStack(),
                'S', PartDefs.PLATFORMEDPLATE().makeStack(),
                'P', PartDefs.PLATE().makeStack(),
                'C', PartDefs.CATHODE().makeStack()
                );

        /** Bus Randomizer **/
        GameRegistry.addRecipe(EnumGate.BusRandomizer.makeStack(),
                "BBB",
                "RGR",
                "BBB",
                'R', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'B', PartDefs.BUNDLEDPLATE().makeStack(),
                'G', PartDefs.ENERGIZEDSILICONCHIP().makeStack()
                );

        /** Bus Converter **/
        GameRegistry.addRecipe(EnumGate.BusConverter.makeStack(),
                "PBP",
                "RSR",
                "PRP",
                'P', PartDefs.PLATE().makeStack(),
                'B', PartDefs.BUNDLEDPLATE().makeStack(),
                'R', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'S', PartDefs.SILICONCHIP().makeStack()
                );

        /** Bus Input Panel **/
        GameRegistry.addRecipe(new ShapedOreRecipe(EnumGate.BusInputPanel.makeStack(),
                "BRB",
                "BIB",
                "BBB",
                'B', PartDefs.BUNDLEDPLATE().makeStack(),
                'R', PartDefs.CONDUCTIVEPLATE().makeStack(),
                'I', PartDefs.oreDictDefinitionIllumar()
        ));
    }
}
