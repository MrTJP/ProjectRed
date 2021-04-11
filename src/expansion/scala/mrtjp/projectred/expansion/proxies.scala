package mrtjp.projectred.expansion

import codechicken.lib.texture.{AtlasRegistrar, SpriteRegistryHelper}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.IProxy
import net.minecraft.util.ResourceLocation
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.client.event.ModelRegistryEvent
import net.minecraftforge.client.model.ModelLoaderRegistry
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent}
import net.minecraftforge.scorge.lang.ScorgeModLoadingContext


class ExpansionProxy extends IProxy {
    override def construct():Unit = {
        super.construct()
        ScorgeModLoadingContext.get.getModEventBus.register(this)
        ExpansionContent.register(ScorgeModLoadingContext.get.getModEventBus)
    }

    override def commonSetup(event:FMLCommonSetupEvent):Unit = {
        super.commonSetup(event)
        CapabilityTeleposedEnderPearl.registerCapability()
    }
}

class ExpansionProxyClient extends ExpansionProxy {
    override def construct():Unit = {
        super.construct()

        ScorgeModLoadingContext.get.getModEventBus.addListener(onModelRegistryEvent)
        val spriteHelper = new SpriteRegistryHelper(ScorgeModLoadingContext.get.getModEventBus)
        spriteHelper.addIIconRegister((registrar:AtlasRegistrar) => {
            RenderSolarPanel.registerIcons(registrar)
        })
    }

    @OnlyIn(Dist.CLIENT)
    @SubscribeEvent
    override def clientSetup(event:FMLClientSetupEvent):Unit = {
        super.clientSetup(event)

        GuiBatteryBox.register()
        GuiChargingBench.register()
        GuiInductiveFurnace.register()
        GuiElectrotineGenerator.register()
        GuiProjectBench.register()
        GuiAutoCrafter.register()
    }

    @OnlyIn(Dist.CLIENT)
    def onModelRegistryEvent(event:ModelRegistryEvent) {
        ModelLoaderRegistry.registerLoader(new ResourceLocation(ProjectRedExpansion.MOD_ID, "solar_panel"), new RenderSolarPanel.Loader)
    }
}

//class ExpansionProxy_server extends IProxy with IPartFactory
//{
//    def preinit()
//    {
//        PacketCustom.assignHandler(ExpansionSPH.channel, ExpansionSPH)
//
//        MultiPartRegistry.registerParts(this, Array(SolarPanelPart.typeID))
//
//        /** Initialization **/
//        itemSolar = new ItemSolarPanel
//        itemEmptybattery = new ItemEmptyBattery
//        itemBattery = new ItemBattery
//        itemJetpack = new ItemJetpack
//        itemScrewdriver = new ItemElectricScrewdriver
//        itemInfusedEnderPearl = new ItemInfusedEnderPearl
//        itemPlan = new ItemPlan
//
//        machine1 = new BlockMachine("machine1", machine1Bakery) //machines
//        machine2 = new BlockMachine("machine2", machine2Bakery) //devices
//
//        enchantmentElectricEfficiency = new EnchantmentElectricEfficiency
//
//        /** Localization **/
//        itemSolar.setUnlocalizedName("projectred.expansion.solarPanel")
//        itemEmptybattery.setUnlocalizedName("projectred.expansion.batteryEmpty")
//        itemBattery.setUnlocalizedName("projectred.expansion.battery")
//        itemJetpack.setUnlocalizedName("projectred.expansion.jetpack")
//        itemScrewdriver.setUnlocalizedName("projectred.expansion.screwdriverElectric")
//        itemInfusedEnderPearl.setUnlocalizedName("projectred.expansion.enderPearlInfused")
//        itemPlan.setUnlocalizedName("projectred.expansion.plan")
//
//        machine1.setUnlocalizedName("projectred.expansion.machine1")
//        machine2.setUnlocalizedName("projectred.expansion.machine2")
//
//        enchantmentElectricEfficiency.setName("projectred.expansion.fuelEfficiency")
//
//        /** Registration **/
//        ForgeRegistries.ITEMS.register(itemSolar.setRegistryName("solar_panel"))
//        ForgeRegistries.ITEMS.register(itemEmptybattery.setRegistryName("empty_battery"))
//        ForgeRegistries.ITEMS.register(itemBattery.setRegistryName("charged_battery"))
//        ForgeRegistries.ITEMS.register(itemJetpack.setRegistryName("jetpack"))
//        ForgeRegistries.ITEMS.register(itemScrewdriver.setRegistryName("electric_screwdriver"))
//        ForgeRegistries.ITEMS.register(itemInfusedEnderPearl.setRegistryName("infused_ender_pearl"))
//        ForgeRegistries.ITEMS.register(itemPlan.setRegistryName("plan"))
//
//        ForgeRegistries.BLOCKS.register(machine1.setRegistryName("machine1"))
//        ForgeRegistries.ITEMS.register(new ItemBlockCore(machine1).setRegistryName(machine1.getRegistryName))
//        ForgeRegistries.BLOCKS.register(machine2.setRegistryName("machine2"))
//        ForgeRegistries.ITEMS.register(new ItemBlockCore(machine2).setRegistryName(machine2.getRegistryName))
//
//        ForgeRegistries.ENCHANTMENTS.register(enchantmentElectricEfficiency.setRegistryName("electric_efficiency"))
//
//        //Machine1 (machines)
//        machine1.addTile(classOf[TileInductiveFurnace], 0)
//        machine1.addTile(classOf[TileElectrotineGenerator], 1)
//
//        //Machine2 (devices)
//        machine2.addTile(classOf[TileBlockBreaker], 0)
//        machine2.addTile(classOf[TileItemImporter], 1)
//        machine2.addTile(classOf[TileBlockPlacer], 2)
//        machine2.addTile(classOf[TileFilteredImporter], 3)
//        machine2.addTile(classOf[TileFireStarter], 4)
//        machine2.addTile(classOf[TileBatteryBox], 5)
//        machine2.addTile(classOf[TileChargingBench], 6)
//        machine2.addTile(classOf[TileTeleposer], 7)
//        machine2.addTile(classOf[TileFrameMotor], 8)
//        machine2.addTile(classOf[TileFrameActuator], 9)
//        machine2.addTile(classOf[TileProjectBench], 10)
//        machine2.addTile(classOf[TileAutoCrafter], 11)
//        machine2.addTile(classOf[TileDiamondBlockBreaker], 12)
//    }
//
//    def init()
//    {
//        CapabilityTeleposedEnderPearl.registerCapability()
//    }
//
//    def postinit()
//    {
//        InductiveFurnaceRecipeLib.init()
//
//        SpacebarServerTracker.register()
//        ForwardServerTracker.register()
//    }
//
//    override def createPart(name:ResourceLocation, client:Boolean):TMultiPart = name match
//    {
//        case SolarPanelPart.typeID => new SolarPanelPart
//        case _ => null
//    }
//}
//
//class ExpansionProxy_client extends ExpansionProxy_server
//{
//    val furnaceGui = 20
//    val generatorGui = 21
//
//    val blockPlacerGui = 22
//    val filteredImporterGui = 23
//    val batteryBoxGui = 24
//    val chargingBenchBui = 25
//    val projectbenchGui = 26
//    val autoCrafterGui = 27
//
//    @SideOnly(Side.CLIENT)
//    override def preinit()
//    {
//        super.preinit()
//        PacketCustom.assignHandler(ExpansionCPH.channel, ExpansionCPH)
//
//        TextureUtils.addIconRegister(RenderSolarPanel)
//        ModelRegistryHelper.registerItemRenderer(itemSolar, RenderSolarPanel)
//
//        ModelLoader.setCustomMeshDefinition(itemEmptybattery, new ItemMeshDefinition {
//            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation("projectred:mechanical/items", "type=empty_battery")
//        })
//        ModelLoader.setCustomMeshDefinition(itemBattery, new ItemMeshDefinition {
//            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation("projectred:mechanical/items", "type=charged_battery")
//        })
//        MCModelBakery.registerItemVariants(itemBattery, new ModelResourceLocation("projectred:mechanical/items", "type=charged_battery"))
//        MCModelBakery.registerItemVariants(itemEmptybattery, new ModelResourceLocation("projectred:mechanical/items", "type=empty_battery"))
//        ModelLoader.setCustomMeshDefinition(itemJetpack, new ItemMeshDefinition {
//            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation("projectred:mechanical/items", "type=jetpack")
//        })
//        MCModelBakery.registerItemVariants(itemJetpack, new ModelResourceLocation("projectred:mechanical/items", "type=jetpack"))
//        ModelLoader.setCustomMeshDefinition(itemScrewdriver, new ItemMeshDefinition {
//            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation("projectred:mechanical/items", "type=screwdriver")
//        })
//        MCModelBakery.registerItemVariants(itemScrewdriver, new ModelResourceLocation("projectred:mechanical/items", "type=screwdriver"))
//        ModelLoader.setCustomModelResourceLocation(itemInfusedEnderPearl, 0, new ModelResourceLocation("projectred:mechanical/items", "type=infused_pearl"))
//        ModelLoader.setCustomMeshDefinition(itemPlan, new ItemMeshDefinition {
//            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation("projectred:mechanical/items", s"type=${if (ItemPlan.hasRecipeInside(stack)) "written" else "blank"}_plan")
//        })
//        for (t <- Array[String]("written", "blank"))
//            MCModelBakery.registerItemVariants(itemPlan, new ModelResourceLocation("projectred:mechanical/items", s"type=${t}_plan"))
//
//        machine1Bakery.registerSubBakery(0, RenderInductiveFurnace, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//                val isWorking = state.getValue(UNLISTED_WORKING_PROPERTY)
//                val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,w=$isWorking,c=$isCharged"
//            }
//        })
//        machine1Bakery.registerSubBakery(1, RenderElectrotineGenerator, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY).asInstanceOf[Boolean]
//                val isBurning = state.getValue(UNLISTED_BURNING_PROPERTY).asInstanceOf[Boolean]
//                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,c=$isCharged,b=$isBurning,s=$side,r=$rotation"
//            }
//        })
//
//        machine2Bakery.registerSubBakery(0, RenderBlockBreaker, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
//                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
//            }
//        })
//        machine2Bakery.registerSubBakery(1, RenderItemImporter, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
//                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
//            }
//        })
//        machine2Bakery.registerSubBakery(2, RenderBlockPlacer, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
//                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
//            }
//        })
//        machine2Bakery.registerSubBakery(3, RenderFilteredImporter, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
//                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
//            }
//        })
//        machine2Bakery.registerSubBakery(4, RenderFireStarter, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
//                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
//            }
//        })
//        machine2Bakery.registerSubBakery(5, RenderBatteryBox, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val charge = state.getValue(UNLISTED_CHARGE_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,c=$charge"
//            }
//        }, new IItemStackKeyGenerator {
//            override def generateKey(stack: ItemStack):String = {
//                val charge = if(stack.hasTagCompound) stack.getTagCompound.getInteger("rstorage") else 0
//                stack.getItem.getRegistryName.toString + "|" + stack.getItemDamage + s",c=$charge"
//            }
//        })
//        machine2Bakery.registerSubBakery(6, RenderChargingBench, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,c=$isCharged"
//            }
//        })
//        machine2Bakery.registerSubBakery(7, RenderTeleposer, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,c=$isCharged"
//            }
//        })
//        machine2Bakery.registerSubBakery(8, RenderFrameMotor, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//                val isWorking = state.getValue(UNLISTED_WORKING_PROPERTY)
//                val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,w=$isWorking,c=$isCharged"
//            }
//        })
//        machine2Bakery.registerSubBakery(9, RenderFrameActuator, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//                val isWorking = state.getValue(UNLISTED_WORKING_PROPERTY)
//                val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,w=$isWorking,c=$isCharged"
//            }
//        })
//        machine2Bakery.registerSubBakery(10, RenderProjectBench)
//        machine2Bakery.registerSubBakery(11, RenderAutoCrafter)
//        machine2Bakery.registerSubBakery(12, RenderDiamondBlockBreaker, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//                val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//                val active = state.getValue(UNLISTED_ACTIVE_PROPERTY)
//                val powered = state.getValue(UNLISTED_POWERED_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,s=$side,r=$rotation,a=$active,p=$powered"
//            }
//        })
//
//        registerBlockToBakery(machine1, machine1Bakery.registerKeyGens(machine1), new Builder().ignore(MultiTileBlock.TILE_INDEX).build())
//        registerBlockToBakery(machine2, machine2Bakery.registerKeyGens(machine2), new Builder().ignore(MultiTileBlock.TILE_INDEX).build())
//    }
//
//    @SideOnly(Side.CLIENT)
//    def registerBlockToBakery(block:Block, iconRegister:IIconRegister, stateMap:IStateMapper) =
//    {
//        val model = new CCBakeryModel()
//        val regLoc = block.getRegistryName
//        ModelLoader.setCustomStateMapper(block, stateMap)
//        ModelLoader.setCustomMeshDefinition(Item.getItemFromBlock(block), new ItemMeshDefinition {
//            override def getModelLocation(stack: ItemStack) = new ModelResourceLocation(regLoc, "normal")
//        })
//        ModelRegistryHelper.register(new ModelResourceLocation(regLoc, "normal"), model)
//        if (iconRegister != null) {
//            TextureUtils.addIconRegister(iconRegister)
//        }
//    }
//
//    @SideOnly(Side.CLIENT)
//    override def init()
//    {
//        super.init()
//    }
//
//    @SideOnly(Side.CLIENT)
//    override def postinit()
//    {
//        super.postinit()
//
//        GuiHandler.register(GuiInductiveFurnace, furnaceGui)
//        GuiHandler.register(GuiBlockPlacer, blockPlacerGui)
//        GuiHandler.register(GuiFilteredImporter, filteredImporterGui)
//        GuiHandler.register(GuiBatteryBox, batteryBoxGui)
//        GuiHandler.register(GuiElectrotineGenerator, generatorGui)
//        GuiHandler.register(GuiChargingBench, chargingBenchBui)
//        GuiHandler.register(GuiProjectBench, projectbenchGui)
//        GuiHandler.register(GuiAutoCrafter, autoCrafterGui)
//
//        SpacebarClientTracker.register()
//        ForwardClientTracker.register()
//
//        ItemJetpack.register()
//    }
//}

//object ExpansionProxy extends ExpansionProxy_client
//
//object SpacebarServerTracker extends TServerKeyTracker
//object SpacebarClientTracker extends TClientKeyTracker
//{
//    override def getTracker = SpacebarServerTracker
//    override def getIsKeyDown = Minecraft.getMinecraft.gameSettings.keyBindJump.isKeyDown
//}
//
//object ForwardServerTracker extends TServerKeyTracker
//object ForwardClientTracker extends TClientKeyTracker
//{
//    override def getTracker = ForwardServerTracker
//    override def getIsKeyDown = Minecraft.getMinecraft.gameSettings.keyBindForward.isKeyDown
//}
