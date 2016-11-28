/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import java.lang.{Character => JChar}

import codechicken.lib.model.ModelRegistryHelper
import codechicken.lib.packet.PacketCustom
import codechicken.lib.texture.TextureUtils
import codechicken.multipart.{IPartFactory, MultiPartRegistry}
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedIntegration._
import mrtjp.projectred.core.{IProxy, PartDefs}
import net.minecraft.init.Blocks
import net.minecraft.item.ItemStack
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import net.minecraftforge.oredict.ShapedOreRecipe

class IntegrationProxy_server extends IProxy with IPartFactory
{
    override def preinit()
    {
        itemPartGate = new ItemPartGate

        import GateDefinition._
        MultiPartRegistry.registerParts(this, Array[String](
            typeSimpleGate, typeComplexGate, typeArrayGate,
            typeBundledGate, typeNeighborGate
        ))
    }

    override def init()
    {
        PacketCustom.assignHandler(IntegrationSPH.channel, IntegrationSPH)

        IntegrationRecipes.initRecipes()
    }

    override def postinit(){}

    override def createPart(name:String, client:Boolean) = name match
    {
        case GateDefinition.typeSimpleGate => new ComboGatePart
        case GateDefinition.typeComplexGate => new SequentialGatePart
        case GateDefinition.typeArrayGate => new ArrayGatePart
        case GateDefinition.typeBundledGate => new BundledGatePart
        case GateDefinition.typeNeighborGate => new SequentialGatePartT
        case _ => null
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class IntegrationProxy_client extends IntegrationProxy_server
{
    val timerGui = 10
    val counterGui = 11

    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()

        ModelRegistryHelper.registerItemRenderer(itemPartGate, GateItemRenderer)
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()

        PacketCustom.assignHandler(IntegrationCPH.channel, IntegrationCPH)

        TextureUtils.addIconRegister(RenderGate)

        GuiHandler.register(GuiTimer, timerGui)
        GuiHandler.register(GuiCounter, counterGui)
    }
}

object IntegrationProxy extends IntegrationProxy_client

object IntegrationRecipes
{
    def initRecipes()
    {
        initGateRecipes()
    }

    private def initGateRecipes()
    {
        /** AND Gate **/
        GameRegistry.addRecipe(GateDefinition.AND.makeStack,
                "ACA",
                "CCC",
                "PWP",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )

        /** OR Gate **/
        GameRegistry.addRecipe(GateDefinition.OR.makeStack,
                "PCP",
                "WCW",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )

        /** NOT Gate**/
        GameRegistry.addRecipe(GateDefinition.NOT.makeStack,
                "PCP",
                "CAC",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )


        /** RS Latch **/
        GameRegistry.addRecipe(GateDefinition.SRLatch.makeStack,
                "ACW",
                "WPW",
                "WCA",
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack
        )

        /** Toggle Latch **/
        GameRegistry.addRecipe(GateDefinition.ToggleLatch.makeStack,
                "CPP",
                "WLW",
                "CPP",
                'C':JChar, PartDefs.CATHODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'L':JChar, new ItemStack(Blocks.LEVER)
        )

        /** Transparent Latch **/
        GameRegistry.addRecipe(GateDefinition.TransparentLatch.makeStack,
                "ACW",
                "CCC",
                "CWP",
                'C':JChar, PartDefs.CATHODE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )

        /** NOR Gate **/
        GameRegistry.addRecipe(GateDefinition.NOR.makeStack,
                "PAP",
                "WCW",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack
        )

        /** NAND Gate **/
        GameRegistry.addRecipe(GateDefinition.NAND.makeStack,
                "AAA",
                "CCC",
                "PWP",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )

        /** XOR Gate **/
        GameRegistry.addRecipe(GateDefinition.XOR.makeStack,
                "AWA",
                "CAC",
                "WCW",
                'A':JChar, PartDefs.ANODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack
        )

        /** XNOR Gate **/
        GameRegistry.addRecipe(GateDefinition.XNOR.makeStack,
                "ACA",
                "CAC",
                "WCW",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )

        /** Buffer Gate **/
        GameRegistry.addRecipe(GateDefinition.Buffer.makeStack,
                "ACA",
                "WCW",
                "PWP",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack
        )

        /** Multiplexer Gate **/
        GameRegistry.addRecipe(GateDefinition.Multiplexer.makeStack,
                "ACA",
                "CPC",
                "ACW",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )

        /** Repeater Gate **/
        GameRegistry.addRecipe(GateDefinition.Repeater.makeStack,
                "PCA",
                "ACP",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )

        /** Timer Gate **/
        GameRegistry.addRecipe(GateDefinition.Timer.makeStack,
                "ACA",
                "WTW",
                "PWP",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'T':JChar, PartDefs.POINTER.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack
        )

        /** Counter Gate **/
        GameRegistry.addRecipe(GateDefinition.Counter.makeStack,
                "PCP",
                "WWT",
                "PCP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'T':JChar, PartDefs.POINTER.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack
        )

        /** Sequencer Gate **/
        GameRegistry.addRecipe(GateDefinition.Sequencer.makeStack,
                "PCP",
                "CTC",
                "PCP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'T':JChar, PartDefs.POINTER.makeStack
        )

        /** Pulse Former Gate **/
        GameRegistry.addRecipe(GateDefinition.Pulse.makeStack,
                "ACA",
                "CAC",
                "WWP",
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack
        )

        /** Randomizer Gate **/
        GameRegistry.addRecipe(GateDefinition.Randomizer.makeStack,
                "PEP",
                "WWW",
                "EWE",
                'P':JChar, PartDefs.PLATE.makeStack,
                'E':JChar, PartDefs.ENERGIZEDSILICONCHIP.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )

        /** State Cell Gate **/
        GameRegistry.addRecipe(GateDefinition.StateCell.makeStack,
                "PAC",
                "WST",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'S':JChar, PartDefs.SILICONCHIP.makeStack,
                'T':JChar, PartDefs.POINTER.makeStack
        )

        /** Synchronizer Gate **/
        GameRegistry.addRecipe(GateDefinition.Synchronizer.makeStack,
                "WCW",
                "SAS",
                "WWW",
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'S':JChar, PartDefs.SILICONCHIP.makeStack,
                'A':JChar, PartDefs.ANODE.makeStack
        )

        /** Light Sensor **/
        GameRegistry.addRecipe(new ShapedOreRecipe(GateDefinition.LightSensor.makeStack,
                "PPP",
                "LLL",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'L':JChar, "dyeBlue",
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
                ))

        /** Rain Sensor **/
        GameRegistry.addRecipe(new ShapedOreRecipe(GateDefinition.RainSensor.makeStack,
                "PPP",
                "SSS",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'S':JChar, "slimeball",
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
                ))

        /** Bus Transceiver **/
        GameRegistry.addRecipe(GateDefinition.BusTransceiver.makeStack,
                "BBB",
                "SPS",
                "BBB",
                'P':JChar, PartDefs.PLATE.makeStack,
                'B':JChar, PartDefs.BUNDLEDPLATE.makeStack,
                'S':JChar, PartDefs.SILICONCHIP.makeStack
        )

        /** Null Cell **/
        GameRegistry.addRecipe(GateDefinition.NullCell.makeStack,
                "PWP",
                "WSW",
                "PWP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.WIREDPLATE.makeStack,
                'S':JChar, PartDefs.PLATFORMEDPLATE.makeStack
        )

        /** Invert Cell **/
        GameRegistry.addRecipe(GateDefinition.InvertCell.makeStack,
                "PWP",
                "WSW",
                "PCP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.WIREDPLATE.makeStack,
                'S':JChar, PartDefs.PLATFORMEDPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack
        )

        /** Buffer Cell **/
        GameRegistry.addRecipe(GateDefinition.BufferCell.makeStack,
                "PWP",
                "WSW",
                "PCC",
                'P':JChar, PartDefs.PLATE.makeStack,
                'W':JChar, PartDefs.WIREDPLATE.makeStack,
                'S':JChar, PartDefs.PLATFORMEDPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack
        )

        /** Comparator **/
        GameRegistry.addRecipe(new ShapedOreRecipe(GateDefinition.Comparator.makeStack,
                "WCW",
                "QWQ",
                "PWP",
                'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack,
                'Q':JChar, "gemQuartz",
                'P':JChar, PartDefs.PLATE.makeStack
        ))

        /** AND Cell **/
        GameRegistry.addRecipe(GateDefinition.ANDCell.makeStack,
                "CwC",
                "WSW",
                "PwC",
                'w':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'W':JChar, PartDefs.WIREDPLATE.makeStack,
                'S':JChar, PartDefs.PLATFORMEDPLATE.makeStack,
                'P':JChar, PartDefs.PLATE.makeStack,
                'C':JChar, PartDefs.CATHODE.makeStack
        )

        /** Bus Randomizer **/
        GameRegistry.addRecipe(GateDefinition.BusRandomizer.makeStack,
                "BBB",
                "RGR",
                "BBB",
                'R':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'B':JChar, PartDefs.BUNDLEDPLATE.makeStack,
                'G':JChar, PartDefs.ENERGIZEDSILICONCHIP.makeStack
        )

        /** Bus Converter **/
        GameRegistry.addRecipe(GateDefinition.BusConverter.makeStack,
                "PBP",
                "RSR",
                "PRP",
                'P':JChar, PartDefs.PLATE.makeStack,
                'B':JChar, PartDefs.BUNDLEDPLATE.makeStack,
                'R':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'S':JChar, PartDefs.SILICONCHIP.makeStack
        )

        /** Bus Input Panel **/
        GameRegistry.addRecipe(new ShapedOreRecipe(GateDefinition.BusInputPanel.makeStack,
                "BRB",
                "BIB",
                "BBB",
                'B':JChar, PartDefs.BUNDLEDPLATE.makeStack,
                'R':JChar, PartDefs.CONDUCTIVEPLATE.makeStack,
                'I':JChar, PartDefs.oreDictDefinitionIllumar
        ))

        /** Stacking Latch **/
        GameRegistry.addRecipe(GateDefinition.StackingLatch.makeStack,
            "PCP",
            "RCR",
            "PCC",
            'P':JChar, PartDefs.PLATE.makeStack,
            'C':JChar, PartDefs.CATHODE.makeStack,
            'R':JChar, PartDefs.WIREDPLATE.makeStack
        )

        /** Segment Display **/
        GameRegistry.addRecipe(new ShapedOreRecipe(GateDefinition.SegmentDisplay.makeStack,
            "PBP",
            "QSQ",
            "PQP",
            'P':JChar, PartDefs.PLATE.makeStack,
            'B':JChar, PartDefs.BUNDLEDPLATE.makeStack,
            'Q':JChar, "gemQuartz",
            'S':JChar, PartDefs.SILICONCHIP.makeStack
        ))

        /** Decoding Randomizer Gate **/
        GameRegistry.addRecipe(GateDefinition.DecRandomizer.makeStack,
            "SCA",
            "CCC",
            "EWE",
            'S':JChar, PartDefs.SILICONCHIP.makeStack,
            'C':JChar, PartDefs.CATHODE.makeStack,
            'A':JChar, PartDefs.ANODE.makeStack,
            'E':JChar, PartDefs.ENERGIZEDSILICONCHIP.makeStack,
            'W':JChar, PartDefs.CONDUCTIVEPLATE.makeStack
        )
    }
}