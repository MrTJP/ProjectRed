package mrtjp.projectred.illumination

import codechicken.lib.colour.EnumColour
import codechicken.lib.datagen.ItemModelProvider
import codechicken.lib.gui.SimpleItemGroup
import codechicken.lib.util.CrashLock
import codechicken.microblock.api.MicroMaterial
import codechicken.multipart.api.part.TMultiPart
import codechicken.multipart.api.{MultiPartType, SimpleMultiPartType}
import mrtjp.projectred.ProjectRedIllumination
import net.minecraft.block.{Block, Blocks}
import net.minecraft.data.DataGenerator
import net.minecraft.item.{BlockItem, Item, ItemStack}
import net.minecraft.state.properties.BlockStateProperties
import net.minecraft.tileentity.TileEntityType
import net.minecraft.util.ResourceLocation
import net.minecraftforge.client.model.generators.{BlockStateProvider, ExistingFileHelper, ModelFile}
import net.minecraftforge.event.RegistryEvent
import net.minecraftforge.eventbus.api.{IEventBus, SubscribeEvent}
import net.minecraftforge.fml.RegistryObject
import net.minecraftforge.fml.event.lifecycle.GatherDataEvent
import net.minecraftforge.registries.{DeferredRegister, ForgeRegistries}

import java.util.function.Supplier

object IlluminationContent
{
    private val LOCK = new CrashLock("Already Initialized.")
    private val BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, ProjectRedIllumination.MOD_ID)
    private val TILES = DeferredRegister.create(ForgeRegistries.TILE_ENTITIES, ProjectRedIllumination.MOD_ID)
    private val ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, ProjectRedIllumination.MOD_ID)
    private val PARTS = DeferredRegister.create(classOf[MultiPartType[_]], ProjectRedIllumination.MOD_ID)
//    private val MICRO_MATERIALS = DeferredRegister.create(classOf[MicroMaterial], ProjectRedIllumination.MOD_ID)

    val illuminationItemGroup = new SimpleItemGroup(ProjectRedIllumination.MOD_ID, () => new ItemStack(Blocks.LANTERN))

    val illumarLampBlocks = new Array[RegistryObject[Block]](16)
    val illumarLampBlockItems = new Array[RegistryObject[Item]](16)
    val illumarLampTiles = new Array[RegistryObject[TileEntityType[IllumarLampTile]]](16)

    val invertedIllumarLampBlocks = new Array[RegistryObject[Block]](16)
    val invertedIllumarLampBlockItems = new Array[RegistryObject[Item]](16)
    val invertedIllumarLampTiles = new Array[RegistryObject[TileEntityType[IllumarLampTile]]](16)

    val illumarButtonParts = new Array[RegistryObject[MultiPartType[_]]](16)
    val illumarButtonItems = new Array[RegistryObject[Item]](16)

    val lightDefinitions = Seq[LightPartDefinition](FixtureLightDefinition, FalloutLightDefinition, LanternLightDefinition, CageLightDefinition)

    for (light <- lightDefinitions)
        light.register(ITEMS, PARTS)

    for (inverted <- Seq(false, true)) for (colour <- 0 until 16) {
        val regName = s"${EnumColour.values()(colour).getName}${ if (inverted) "_inverted" else ""}_illumar_lamp"

        val blockReg = if (inverted) invertedIllumarLampBlocks else illumarLampBlocks
        val itemReg = if (inverted) invertedIllumarLampBlockItems else illumarLampBlockItems
        val tileReg = if (inverted) invertedIllumarLampTiles else illumarLampTiles

        blockReg(colour) = BLOCKS.register(regName, illumarBlockFactory(colour, inverted))
        itemReg(colour) = ITEMS.register(regName, illumarBlockItemFactory(blockReg(colour)))
        tileReg(colour) = TILES.register(regName, illumarLampTileFactory(colour, inverted))

//        if (inverted)
//            MICRO_MATERIALS.register(regName, () => new LightMicroMaterial(blockReg(colour)))
    }

    for (colour <- 0 until 16) {
        val regName = s"${EnumColour.values()(colour).getName}_illumar_button"

        illumarButtonParts(colour) = PARTS.register(regName, () =>
            new SimpleMultiPartType[TMultiPart](_ =>
                new LightButtonPart(IlluminationContent.illumarButtonParts(colour).get(), colour).asInstanceOf[TMultiPart]))

        illumarButtonItems(colour) = ITEMS.register(regName, () => new ItemPartButton(illumarButtonParts(colour)))
    }


    def illumarBlockFactory(colour:Int, inverted:Boolean):Supplier[Block] = () =>
        new IllumarLampBlock((if (inverted) invertedIllumarLampTiles else illumarLampTiles)(colour), colour, inverted)

    def illumarBlockItemFactory(blockFactory:Supplier[Block]):Supplier[Item] = () =>
        new BlockItem(blockFactory.get, new Item.Properties().group(illuminationItemGroup))

    def illumarLampTileFactory(colour:Int, inverted:Boolean):Supplier[TileEntityType[IllumarLampTile]] = () =>
        TileEntityType.Builder.create[IllumarLampTile](() =>
            new IllumarLampTile((if (inverted) invertedIllumarLampTiles else illumarLampTiles)(colour).get, colour, inverted),
            (if (inverted) invertedIllumarLampBlocks else illumarLampBlocks)(colour).get).build(null)

    @SubscribeEvent
    def onRegisterMicroMaterials(event: RegistryEvent.Register[MicroMaterial]) {
        for (i <- 0 until 16) {
            event.getRegistry.register(new LightMicroMaterial(invertedIllumarLampBlocks(i)))
        }
    }

    def register(bus:IEventBus):Unit = {
        LOCK.lock()
        BLOCKS.register(bus)
        TILES.register(bus)
        ITEMS.register(bus)
        PARTS.register(bus)
//        MICRO_MATERIALS.register(bus)
        bus.register(DataGen)
        bus.register(this)
    }
}

private object DataGen {
    @SubscribeEvent
    def gatherDataGenerators(event: GatherDataEvent):Unit = {
        val gen = event.getGenerator
        val helper = event.getExistingFileHelper
        if (event.includeClient()) {
            gen.addProvider(new ItemModels(gen, helper))
            gen.addProvider(new BlockStates(gen, helper))
        }
        if (event.includeServer()) {
//            gen.addProvider(new Recipes(gen))
        }
    }
}

private class ItemModels(gen: DataGenerator, fileHelper: ExistingFileHelper) extends ItemModelProvider(gen, ProjectRedIllumination.MOD_ID, fileHelper) {

    override def getName = "ProjectRed-Illumination Item Models."

    override protected def registerModels():Unit = {

        import IlluminationContent._

        for (light <- lightDefinitions) {
            val modelFile = getExistingFile(new ResourceLocation(ProjectRedIllumination.MOD_ID, light.getItemModelPath))
            for (itemRegObj <- light.itemRegObjects ++ light.invertedItemRegObjects)
                getSimple(itemRegObj).texture(null).parent(modelFile)
        }

        for (colour <- 0 until 16) {
            makeLamp(illumarLampBlockItems(colour).get(),
                invertedIllumarLampBlockItems(colour).get())
        }
    }

    private def extend(rl:ResourceLocation, prefix:String, suffix:String) =
        new ResourceLocation(rl.getNamespace, prefix + rl.getPath + suffix)

    private def makeLamp(lamp:Item, invertedLamp:Item) {
        val offModel = new ModelFile.UncheckedModelFile(extend(lamp.getRegistryName, "block/", "_off"))
        val onModel = new ModelFile.UncheckedModelFile(extend(lamp.getRegistryName, "block/", "_on"))
        generated(lamp).texture(null).parent(offModel)
        generated(invertedLamp).texture(null).parent(onModel)
    }
}

private class BlockStates(gen:DataGenerator, fileHelper:ExistingFileHelper) extends BlockStateProvider(gen, ProjectRedIllumination.MOD_ID, fileHelper) {

    override def getName:String = "ProjectRed-Illumination Block Models"

    override protected def registerStatesAndModels():Unit = {
        import IlluminationContent._

        for (colour <- 0 until 16) {
            makeLamp(illumarLampBlocks(colour).get(),
                invertedIllumarLampBlocks(colour).get(),
                EnumColour.values()(colour))
        }
    }

    private def extend(rl:ResourceLocation, suffix:String) =
        new ResourceLocation(rl.getNamespace, rl.getPath + suffix)

    private def makeLamp(lamp:Block, invertedLamp:Block, colour:EnumColour):Unit = {
        val offLocation = new ResourceLocation(ProjectRedIllumination.MOD_ID, "block/illumar_lamp/" + colour.getName + "_off")
        val onLocation = new ResourceLocation(ProjectRedIllumination.MOD_ID, "block/illumar_lamp/" + colour.getName + "_on")
        val offModel = models.cubeAll(extend(lamp.getRegistryName, "_off").toString, offLocation)
        val onModel = models.cubeAll(extend(lamp.getRegistryName, "_on").toString, onLocation)

        getVariantBuilder(lamp)
                .partialState().`with`(BlockStateProperties.LIT, Boolean.box(false)).modelForState().modelFile(offModel).addModel()
                .partialState().`with`(BlockStateProperties.LIT, Boolean.box(true)).modelForState().modelFile(onModel).addModel()

        getVariantBuilder(invertedLamp)
                .partialState().`with`(BlockStateProperties.LIT, Boolean.box(false)).modelForState().modelFile(offModel).addModel()
                .partialState().`with`(BlockStateProperties.LIT, Boolean.box(true)).modelForState().modelFile(onModel).addModel()
    }

}