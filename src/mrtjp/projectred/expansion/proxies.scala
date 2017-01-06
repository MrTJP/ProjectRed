package mrtjp.projectred.expansion

import java.lang.{Character => JC}

import codechicken.lib.data.MCDataInput
import codechicken.lib.model.ModelRegistryHelper
import codechicken.lib.model.blockbakery.{CCBakeryModel, IBlockStateKeyGenerator, IItemStackKeyGenerator}
import codechicken.lib.packet.PacketCustom
import codechicken.lib.texture.TextureUtils
import codechicken.lib.texture.TextureUtils.IIconRegister
import codechicken.multipart.{IDynamicPartFactory, MultiPartRegistry, TMultiPart}
import mrtjp.core.block.{ItemBlockCore, MultiTileBlock}
import mrtjp.core.data.{TClientKeyTracker, TServerKeyTracker}
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedExpansion._
import mrtjp.projectred.core.{Configurator, IProxy, PartDefs}
import mrtjp.projectred.expansion.BlockProperties._
import net.minecraft.block.Block
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.ItemMeshDefinition
import net.minecraft.client.renderer.block.model.{ModelBakery, ModelResourceLocation}
import net.minecraft.client.renderer.block.statemap.IStateMapper
import net.minecraft.client.renderer.block.statemap.StateMap.Builder
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import net.minecraftforge.oredict.{ShapedOreRecipe, ShapelessOreRecipe}

class ExpansionProxy_server extends IProxy with IDynamicPartFactory
{
    def preinit()
    {
        PacketCustom.assignHandler(ExpansionSPH.channel, ExpansionSPH)

        MultiPartRegistry.registerParts(this, Array("projectred-expansion:solar_panel"))

        /** Initialization **/
        itemSolar = new ItemSolarPanel
        itemEmptybattery = new ItemEmptyBattery
        itemBattery = new ItemBattery
        itemJetpack = new ItemJetpack
        itemScrewdriver = new ItemElectricScrewdriver
        itemInfusedEnderPearl = new ItemInfusedEnderPearl
        itemPlan = new ItemPlan

        machine1 = new BlockMachine("machine1", machine1Bakery) //machines
        machine2 = new BlockMachine("machine2", machine2Bakery) //devices

        enchantmentElectricEfficiency = new EnchantmentElectricEfficiency

        /** Localization **/
        itemSolar.setUnlocalizedName("projectred.expansion.solarPanel")
        itemEmptybattery.setUnlocalizedName("projectred.expansion.batteryEmpty")
        itemBattery.setUnlocalizedName("projectred.expansion.battery")
        itemJetpack.setUnlocalizedName("projectred.expansion.jetpack")
        itemScrewdriver.setUnlocalizedName("projectred.expansion.screwdriverElectric")
        itemInfusedEnderPearl.setUnlocalizedName("projectred.expansion.enderPearlInfused")
        itemPlan.setUnlocalizedName("projectred.expansion.plan")

        machine1.setUnlocalizedName("projectred.expansion.machine1")
        machine2.setUnlocalizedName("projectred.expansion.machine2")

        enchantmentElectricEfficiency.setName("projectred.expansion.fuelEfficiency")

        /** Registration **/
        GameRegistry.register(itemSolar.setRegistryName("solar_panel"))
        GameRegistry.register(itemEmptybattery.setRegistryName("empty_battery"))
        GameRegistry.register(itemBattery.setRegistryName("charged_battery"))
        GameRegistry.register(itemJetpack.setRegistryName("jetpack"))
        GameRegistry.register(itemScrewdriver.setRegistryName("electric_screwdriver"))
        GameRegistry.register(itemInfusedEnderPearl.setRegistryName("infused_ender_pearl"))
        GameRegistry.register(itemPlan.setRegistryName("plan"))

        GameRegistry.register(machine1.setRegistryName("machine1"))
        GameRegistry.register(new ItemBlockCore(machine1).setRegistryName(machine1.getRegistryName))
        GameRegistry.register(machine2.setRegistryName("machine2"))
        GameRegistry.register(new ItemBlockCore(machine2).setRegistryName(machine2.getRegistryName))

        GameRegistry.register(enchantmentElectricEfficiency.setRegistryName("electric_efficiency"))

        //Machine1 (machines)
        machine1.addTile(classOf[TileInductiveFurnace], 0)
        machine1.addTile(classOf[TileElectrotineGenerator], 1)

        //Machine2 (devices)
        machine2.addTile(classOf[TileBlockBreaker], 0)
        machine2.addTile(classOf[TileItemImporter], 1)
        //machine2.addTile(classOf[TileBlockPlacer], 2)
        machine2.addTile(classOf[TileFilteredImporter], 3)
        machine2.addTile(classOf[TileFireStarter], 4)
        machine2.addTile(classOf[TileBatteryBox], 5)
        machine2.addTile(classOf[TileChargingBench], 6)
        //machine2.addTile(classOf[TileTeleposer], 7)
        //machine2.addTile(classOf[TileFrameMotor], 8)
        //machine2.addTile(classOf[TileFrameActuator], 9)
        machine2.addTile(classOf[TileProjectBench], 10)
        machine2.addTile(classOf[TileAutoCrafter], 11)
        machine2.addTile(classOf[TileDiamondBlockBreaker], 12)
    }

    def init()
    {

    }

    def postinit()
    {
        ExpansionRecipes.initRecipes()

        SpacebarServerTracker.register()
        ForwardServerTracker.register()

        //MinecraftForge.EVENT_BUS.register(TeleposedEnderPearlProperty)
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"

    override def createPart(name:String, nbt:NBTTagCompound) = createPart(name)
    override def createPart(name:String, packet:MCDataInput) = createPart(name)

    def createPart(name:String):TMultiPart = name match
    {
        case "pr_solar" => new SolarPanelPart
    }
}

class ExpansionProxy_client extends ExpansionProxy_server
{
    val furnaceGui = 20
    val generatorGui = 21

    val blockPlacerGui = 22
    val filteredImporterGui = 23
    val batteryBoxGui = 24
    val chargingBenchBui = 25
    val projectbenchGui = 26
    val autoCrafterGui = 27

    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
        PacketCustom.assignHandler(ExpansionCPH.channel, ExpansionCPH)

        TextureUtils.addIconRegister(RenderSolarPanel)
        ModelRegistryHelper.registerItemRenderer(itemSolar, RenderSolarPanel)

        ModelLoader.setCustomMeshDefinition(itemEmptybattery, new ItemMeshDefinition {
            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation("projectred:mechanical/items", "type=empty_battery")
        })
        ModelLoader.setCustomMeshDefinition(itemBattery, new ItemMeshDefinition {
            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation("projectred:mechanical/items", "type=charged_battery")
        })
        ModelBakery.registerItemVariants(itemBattery, new ModelResourceLocation("projectred:mechanical/items", "type=charged_battery"))
        ModelBakery.registerItemVariants(itemEmptybattery, new ModelResourceLocation("projectred:mechanical/items", "type=empty_battery"))
        ModelLoader.setCustomMeshDefinition(itemJetpack, new ItemMeshDefinition {
            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation("projectred:mechanical/items", "type=jetpack")
        })
        ModelBakery.registerItemVariants(itemJetpack, new ModelResourceLocation("projectred:mechanical/items", "type=jetpack"))
        ModelLoader.setCustomMeshDefinition(itemScrewdriver, new ItemMeshDefinition {
            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation("projectred:mechanical/items", "type=screwdriver")
        })
        ModelBakery.registerItemVariants(itemScrewdriver, new ModelResourceLocation("projectred:mechanical/items", "type=screwdriver"))
        ModelLoader.setCustomModelResourceLocation(itemInfusedEnderPearl, 0, new ModelResourceLocation("projectred:mechanical/items", "type=infused_pearl"))
        ModelLoader.setCustomMeshDefinition(itemPlan, new ItemMeshDefinition {
            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation("projectred:mechanical/items", s"type=${if (ItemPlan.hasRecipeInside(stack)) "written" else "blank"}_plan")
        })
        for (t <- Array[String]("written", "blank"))
            ModelBakery.registerItemVariants(itemPlan, new ModelResourceLocation("projectred:mechanical/items", s"type=${t}_plan"))

        machine1Bakery.registerSubBakery(0, RenderInductiveFurnace, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
                val isWorking = state.getValue(UNLISTED_WORKING_PROPERTY)
                val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
                val meta = state.getBlock.getMetaFromState(state)
                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,w=$isWorking,c=$isCharged"
            }
        })
        machine1Bakery.registerSubBakery(1, RenderElectrotineGenerator, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY).asInstanceOf[Boolean]
                val isBurning = state.getValue(UNLISTED_BURNING_PROPERTY).asInstanceOf[Boolean]
                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
                val meta = state.getBlock.getMetaFromState(state)
                state.getBlock.getRegistryName.toString + s",meta=$meta,c=$isCharged,b=$isBurning,s=$side,r=$rotation"
            }
        })

        machine2Bakery.registerSubBakery(0, RenderBlockBreaker, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
                val meta = state.getBlock.getMetaFromState(state)
                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
            }
        })
        machine2Bakery.registerSubBakery(1, RenderItemImporter, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
                val meta = state.getBlock.getMetaFromState(state)
                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
            }
        })
        machine2Bakery.registerSubBakery(2, RenderBlockPlacer, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
                val meta = state.getBlock.getMetaFromState(state)
                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
            }
        })
        machine2Bakery.registerSubBakery(3, RenderFilteredImporter, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
                val meta = state.getBlock.getMetaFromState(state)
                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
            }
        })
        machine2Bakery.registerSubBakery(4, RenderFireStarter, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
                val meta = state.getBlock.getMetaFromState(state)
                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
            }
        })
        machine2Bakery.registerSubBakery(5, RenderBatteryBox, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val charge = state.getValue(UNLISTED_CHARGE_PROPERTY)
                val meta = state.getBlock.getMetaFromState(state)
                state.getBlock.getRegistryName.toString + s",meta=$meta,c=$charge"
            }
        }, new IItemStackKeyGenerator {
            override def generateKey(stack: ItemStack):String = {
                val charge = if(stack.hasTagCompound) stack.getTagCompound.getInteger("rstorage") else 0
                stack.getItem.getRegistryName.toString + "|" + stack.getItemDamage + s",c=$charge"
            }
        })
        machine2Bakery.registerSubBakery(6, RenderChargingBench, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
                val meta = state.getBlock.getMetaFromState(state)
                state.getBlock.getRegistryName.toString + s",meta=$meta,c=$isCharged"
            }
        })

        machine2Bakery.registerSubBakery(10, RenderProjectBench)
        machine2Bakery.registerSubBakery(11, RenderAutoCrafter)
        machine2Bakery.registerSubBakery(12, RenderDiamondBlockBreaker, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
                val meta = state.getBlock.getMetaFromState(state)
                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
            }
        })

        registerBlockToBakery(machine1, machine1Bakery.registerKeyGens(machine1), new Builder().ignore(MultiTileBlock.TILE_INDEX).build())
        registerBlockToBakery(machine2, machine2Bakery.registerKeyGens(machine2), new Builder().ignore(MultiTileBlock.TILE_INDEX).build())
    }

    @SideOnly(Side.CLIENT)
    def registerBlockToBakery(block:Block, iconRegister:IIconRegister, stateMap:IStateMapper) =
    {
        val model = new CCBakeryModel("")
        val regLoc = block.getRegistryName
        ModelLoader.setCustomStateMapper(block, stateMap)
        ModelLoader.setCustomMeshDefinition(Item.getItemFromBlock(block), new ItemMeshDefinition {
            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation(regLoc, "normal")
        })
        ModelRegistryHelper.register(new ModelResourceLocation(regLoc, "normal"), model)
        if (iconRegister != null) {
            TextureUtils.addIconRegister(iconRegister)
        }
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
    }

    @SideOnly(Side.CLIENT)
    override def postinit()
    {
        super.postinit()

        GuiHandler.register(GuiInductiveFurnace, furnaceGui)
        GuiHandler.register(GuiBlockPlacer, blockPlacerGui)
        GuiHandler.register(GuiFilteredImporter, filteredImporterGui)
        GuiHandler.register(GuiBatteryBox, batteryBoxGui)
        GuiHandler.register(GuiElectrotineGenerator, generatorGui)
        GuiHandler.register(GuiChargingBench, chargingBenchBui)
        GuiHandler.register(GuiProjectBench, projectbenchGui)
        GuiHandler.register(GuiAutoCrafter, autoCrafterGui)

        SpacebarClientTracker.register()
        ForwardClientTracker.register()

        ItemJetpack.register()
    }
}

object ExpansionProxy extends ExpansionProxy_client

object SpacebarServerTracker extends TServerKeyTracker
object SpacebarClientTracker extends TClientKeyTracker
{
    override def getTracker = SpacebarServerTracker
    override def getIsKeyDown = Minecraft.getMinecraft.gameSettings.keyBindJump.isKeyDown
}

object ForwardServerTracker extends TServerKeyTracker
object ForwardClientTracker extends TClientKeyTracker
{
    override def getTracker = ForwardServerTracker
    override def getIsKeyDown = Minecraft.getMinecraft.gameSettings.keyBindForward.isKeyDown
}

object ExpansionRecipes
{
    def initRecipes()
    {
        InductiveFurnaceRecipeLib.init()
        initItemRecipes()
        initMachineRecipes()
        initDeviceRecipes()
        initMiscRecipes()
    }

    private def initItemRecipes()
    {
        //Battery
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemBattery),
            "ete","ece", "ete",
            'e':JC, PartDefs.ELECTROTINE.makeStack,
            't':JC, "ingotTin",
            'c':JC, "ingotCopper"
        ))

        //Electric Screwdriver
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemScrewdriver),
            "i  ", " s ", "  b",
            'i':JC, "ingotIron",
            's':JC, "gemSapphire",
            'b':JC, itemBattery
        ))

        //Jetpack
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemJetpack),
            "b b","bcb","eae",
            'b':JC, itemBattery,
            'c':JC, Items.DIAMOND_CHESTPLATE,
            'e':JC, Items.EMERALD,
            'a':JC, new ItemStack(machine2, 1, 5)
        ))

        //Recipe Plan
        GameRegistry.addRecipe(new ShapelessOreRecipe(new ItemStack(itemPlan), "dyeBlue", Items.PAPER))
    }

    private def initMachineRecipes()
    {
        //Inductive Furnace
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine1, 1, 0),
            "bbb", "b b", "iei",
            'b':JC, Blocks.BRICK_BLOCK,
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Electrotine generator
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine1, 1, 1),
            "bbb", "a a", "cec",
            'b':JC, Blocks.BRICK_BLOCK,
            'a':JC, new ItemStack(itemBattery),
            'c':JC, Blocks.CLAY,
            'e':JC, "ingotElectrotineAlloy"
        ))
    }

    private def initDeviceRecipes()
    {
        //Block Breaker
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 0),
            "sas", "sps", "srs",
            's':JC, Blocks.COBBLESTONE,
            'a':JC, Items.IRON_PICKAXE,
            'p':JC, Blocks.PISTON,
            'r':JC, Items.REDSTONE
        ))

        //Item Importer
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 1),
            "www", "sps", "srs",
            'w':JC, "slabWood",
            's':JC, Blocks.COBBLESTONE,
            'p':JC, Blocks.PISTON,
            'r':JC, Items.REDSTONE
        ))

        //Block Placer
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 2),
            "ihi","cpc", "crc",
            'i':JC, "ingotIron",
            'h':JC, Blocks.CHEST,
            'c':JC, Blocks.COBBLESTONE,
            'p':JC, Blocks.PISTON,
            'r':JC, Items.REDSTONE
        ))

        //Filtered Importer
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 3),
            "tct", "gig", "sgs",
            't':JC, new ItemStack(Blocks.STONE_SLAB,1, 0),
            'c':JC, Blocks.CHEST,
            'g':JC, "ingotGold",
            'i':JC, new ItemStack(machine2, 1, 1),
            's':JC, Blocks.COBBLESTONE
        ))

        //Fire Starter
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 4),
            "nfn", "cpc", "crc",
            'n':JC, Blocks.NETHERRACK,
            'f':JC, Items.FLINT_AND_STEEL,
            'c':JC, Blocks.COBBLESTONE,
            'p':JC, new ItemStack(machine2, 1, 2),
            'r':JC, Items.REDSTONE
        ))

        //Battery Box
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 5),
            "bwb","bib","iei",
            'b':JC, new ItemStack(itemBattery),
            'w':JC, "plankWood",
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Solar Panel
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemSolar),
            "sss","iwi","wew",
            's':JC, PartDefs.ELECTROSILICON.makeStack,
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy",
            'w':JC, "slabWood"
        ))

        //Charging Bench
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 6),
            "scs","wbw","iei",
            's':JC, Blocks.STONE,
            'c':JC, PartDefs.COPPERCOIL.makeStack,
            'w':JC, "plankWood",
            'b':JC, new ItemStack(itemBattery),
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Teleposer
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 7),
            "odo","wbw","iei",
            'o':JC, Blocks.OBSIDIAN,
            'd':JC, Items.DIAMOND,
            'w':JC, "plankWood",
            'b':JC, Items.ENDER_PEARL,
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Frame Motor
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 8),
            "wiw","cmc","iei",
            'w':JC, "plankWood",
            'i':JC, "ingotIron",
            'c':JC, PartDefs.COPPERCOIL.makeStack,
            'm':JC, PartDefs.MOTOR.makeStack,
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Frame Linear Actuator
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 9),
            "wiw","cic","cec",
            'w':JC, "plankWood",
            'i':JC, "ingotIron",
            'c':JC, PartDefs.COPPERCOIL.makeStack,
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Project Bench
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 10),
            "sss","wbw","wcw",
            's':JC, Blocks.STONE,
            'w':JC, "plankWood",
            'b':JC, Blocks.CRAFTING_TABLE,
            'c':JC, Blocks.CHEST
        ))

        //Auto Crafting Bench
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 11),
            "sbs","ici","wew",
            's':JC, Blocks.STONE,
            'b':JC, Blocks.CRAFTING_TABLE,
            'w':JC, "plankWood",
            'i':JC, "ingotIron",
            'c':JC, Blocks.CHEST,
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Diamond Block Breaker
        if (Configurator.enableDiamondBlockBreaker) {
            GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 12),
                "sas", "sps", "srs",
                's':JC, Blocks.COBBLESTONE,
                'a':JC, Items.DIAMOND_PICKAXE,
                'p':JC, Blocks.PISTON,
                'r':JC, Items.REDSTONE
            ))
        }

    }

    private def initMiscRecipes()
    {
    }
}
