package mrtjp.projectred.core

import codechicken.lib.packet.PacketCustom
import mrtjp.projectred.ProjectRedCore._
import mrtjp.projectred.core.PartDefs._
import net.minecraft.client.renderer.block.model.ModelResourceLocation
import net.minecraft.init.Blocks
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.fml.common.registry.{ForgeRegistries, GameRegistry}
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import net.minecraftforge.oredict.OreDictionary

class CoreProxy_server extends IProxy
{
    def preinit()
    {
        MinecraftForge.EVENT_BUS.register(Configurator)

        /* Item Registration */
        itemPart = new ItemPart
        itemPart.setUnlocalizedName("projectred.core.itemResource")
        ForgeRegistries.ITEMS.register(itemPart.setRegistryName("resource_item"))

        itemDrawPlate = new ItemDrawPlate
        itemDrawPlate.setUnlocalizedName("projectred.core.drawplate")
        ForgeRegistries.ITEMS.register(itemDrawPlate.setRegistryName("drawplate"))

        itemScrewdriver = new ItemScrewdriver
        itemScrewdriver.setUnlocalizedName("projectred.core.screwdriver")
        ForgeRegistries.ITEMS.register(itemScrewdriver.setRegistryName("screwdriver"))

        itemMultimeter = new ItemMultimeter
        itemMultimeter.setUnlocalizedName("projectred.core.multimeter")
        ForgeRegistries.ITEMS.register(itemMultimeter.setRegistryName("multimeter"))

        /* OreDictionary */
        for (i <- 0 until 16)
            OreDictionary.registerOre(PartDefs.oreDictDefinitionIllumar, PartDefs.ILLUMARS(i).makeStack)

        OreDictionary.registerOre("gemRuby", PartDefs.RUBY.makeStack)
        OreDictionary.registerOre("gemSapphire", PartDefs.SAPPHIRE.makeStack)
        OreDictionary.registerOre("gemPeridot", PartDefs.PERIDOT.makeStack)
        OreDictionary.registerOre("ingotRedAlloy", PartDefs.REDINGOT.makeStack)
        OreDictionary.registerOre("ingotCopper", PartDefs.COPPERINGOT.makeStack)
        OreDictionary.registerOre("ingotTin", PartDefs.TININGOT.makeStack)
        OreDictionary.registerOre("ingotSilver", PartDefs.SILVERINGOT.makeStack)
        OreDictionary.registerOre("ingotElectrotineAlloy", PartDefs.ELECTROTINEINGOT.makeStack)
        OreDictionary.registerOre("dustElectrotine", PartDefs.ELECTROTINE.makeStack)
    }

    def init()
    {
        PacketCustom.assignHandler(CoreSPH.channel, CoreSPH)

        /* Smelting */

        // Circut Plate
        GameRegistry.addSmelting(Blocks.STONE, PLATE.makeStack(2), 0f)
        // Silicon Boule
        GameRegistry.addSmelting(SANDYCOALCOMPOUND.makeStack, SILICONBOULE.makeStack, 0)
        // Infused Silicon
        GameRegistry.addSmelting(REDSILICONCOMPOUND.makeStack, INFUSEDSILICON.makeStack, 0)
        // Energized Silicon
        GameRegistry.addSmelting(GLOWINGSILICONCOMPOUND.makeStack, ENERGIZEDSILICON.makeStack, 0)
        // Red Alloy Ingot
        GameRegistry.addSmelting(REDIRONCOMPOUND.makeStack, REDINGOT.makeStack, 0)
        // Electrotine Ingot
        GameRegistry.addSmelting(ELECTROTINEIRONCOMPOUND.makeStack, ELECTROTINEINGOT.makeStack, 0)
        // Electrosilicon
        GameRegistry.addSmelting(ELECTROTINESILICONCOMPOUND.makeStack, ELECTROSILICON.makeStack, 0)
    }

    def postinit(){}
}

class CoreProxy_client extends CoreProxy_server
{
    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
        //RecipeDumper.load()

        for (i <- PartDefs.values) i.setCustomModelResourceLocations()
        ModelLoader.setCustomModelResourceLocation(itemDrawPlate, 0, new ModelResourceLocation("projectred:base/tools", "type=draw_plate"))
        ModelLoader.setCustomModelResourceLocation(itemScrewdriver, 0, new ModelResourceLocation("projectred:base/tools", "type=screwdriver"))
        ModelLoader.setCustomModelResourceLocation(itemMultimeter, 0, new ModelResourceLocation("projectred:base/tools", "type=multimeter"))
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        PacketCustom.assignHandler(CoreCPH.channel, CoreCPH)
    }

    @SideOnly(Side.CLIENT)
    override def postinit()
    {
        super.postinit()
        MinecraftForge.EVENT_BUS.register(RenderHalo)
    }
}

object CoreProxy extends CoreProxy_client
