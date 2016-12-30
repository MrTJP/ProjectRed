package mrtjp.projectred.transportation

import java.util.{List => JList}

import codechicken.lib.vec.Vector3
import codechicken.multipart.{MultiPartRegistry, TItemMultiPart}
import com.mojang.realmsclient.gui.ChatFormatting
import mrtjp.core.item.{ItemCore, ItemDefinition}
import mrtjp.projectred.ProjectRedCore._
import mrtjp.projectred.ProjectRedTransportation
import mrtjp.projectred.transportation.ChipType.ChipType
import net.minecraft.block.SoundType
import net.minecraft.client.renderer.block.model.ModelResourceLocation
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{EnumActionResult, EnumFacing, EnumHand, ResourceLocation}
import net.minecraft.world.{IBlockAccess, World}
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import org.lwjgl.input.Keyboard

import scala.collection.mutable.ListBuffer

class ItemPartPipe extends ItemCore with TItemMultiPart
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedTransportation.tabTransportation)

    /**
      * Create a new part based on the placement information parameters.
      */
    override def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockPos, side:Int, vhit:Vector3) =
    {
        val pdef = PipeDefs.values(item.getItemDamage)
        val p = MultiPartRegistry.loadPart(pdef.partname, null:NBTTagCompound).asInstanceOf[PayloadPipePart[_]]
        if (p != null) p.preparePlacement(side, item.getItemDamage)
        p
    }

    @SideOnly(Side.CLIENT)
    override def getSubItems(item:Item, tab:CreativeTabs, list:JList[ItemStack])
    {
        for (t <- PipeDefs.values) list.add(t.makeStack)
    }

    override def getPlacementSound(item:ItemStack) = SoundType.GLASS
}

object PipeDefs extends ItemDefinition
{
    override type EnumVal = PipeVal
    override def getItem = ProjectRedTransportation.itemPartPipe

    val BASIC = new PipeVal("projectred-transporation:pipe", "basic", "rs")
    val ROUTEDJUNCTION = new PipeVal("projectred-transporation:routed_pipe", "routedjunc",
        "routed", "unrouted", "routedconn", "unroutedconn")
    val ROUTEDINTERFACE = new PipeVal("projectred-transporation:interface_pipe", "routedint")
    //val ROUTEDCRAFTING = new PipeVal("pr_rcrafting", "routedcrafting") /** deprecated **/
    val ROUTEDREQUEST = new PipeVal("projectred-transporation:request_pipe", "routedrequest")
    //val ROUTEDEXTENSION = new PipeVal("pr_rextension", "routedextension") /** deprecated **/
    val ROUTEDFIREWALL = new PipeVal("projectred-transporation:firewall_pipe", "routedfire")
    val PRESSURETUBE = new PipeVal("projectred-transporation:pressure_tube", Seq("pressuretube")++(0 to 15 map{"colour/colour_"+_}):_*)
    val RESISTANCETUBE = new PipeVal("projectred-transporation:resustance_tube", "resistancetube")
    val NETWORKVALVE = new PipeVal("projectred-transporation:netvalve_pipe", "netvalve_blocked", "netvalve_in", "netvalve_out", "netvalve_inout")
    val NETWORKLATENCY = new PipeVal("projectred-transporation:netlatency_pipe", "netlatency")

    class PipeVal(val partname:String, val textures:String*) extends ItemDef(partname)
    {
        var sprites:Array[TextureAtlasSprite] = _

        def registerIcon(map:TextureMap)
        {
            sprites = new Array[TextureAtlasSprite](textures.length)
            if (textures.nonEmpty) for (i <- 0 until textures.length)
                sprites(i) = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/pipes/"+textures(i)))
        }
    }
}

class ItemRoutingChip extends ItemCore
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedTransportation.tabTransportation)

    override def getSubItems(i:Item, tab:CreativeTabs, list:JList[ItemStack])
    {
        for (c <- RoutingChipDefs.values) list.add(c.makeStack)
    }

    override def addInformation(stack:ItemStack, player:EntityPlayer, list:JList[String], par4:Boolean)
    {
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT)) if (ItemRoutingChip.hasChipInside(stack))
        {
            import scala.collection.JavaConversions._
            val r = ItemRoutingChip.loadChipFromItemStack(stack)
            val s = new ListBuffer[String]
            r.infoCollection(s)
            list.addAll(s)
        }
        else list.add(ChatFormatting.GRAY+"not configured")
    }

    override def onItemRightClick(stack:ItemStack, world:World, player:EntityPlayer, hand:EnumHand) =
    {
        if (!world.isRemote && ItemRoutingChip.isValidChip(stack))
        {
            val r = ItemRoutingChip.loadChipFromItemStack(stack)
            r.openGui(player)
        }
        super.onItemRightClick(stack, world, player, hand)
    }


    override def onItemUse(stack:ItemStack, player:EntityPlayer, world:World, pos:BlockPos, hand:EnumHand, facing:EnumFacing, hitX:Float, hitY:Float, hitZ:Float) =
    {
        if (!world.isRemote && ItemRoutingChip.isValidChip(stack))
        {
            val r = ItemRoutingChip.loadChipFromItemStack(stack)
            r.openGui(player)
        }
        EnumActionResult.SUCCESS
    }

    override def doesSneakBypassUse(stack:ItemStack, world:IBlockAccess, pos:BlockPos, player:EntityPlayer) = true
}

object ItemRoutingChip
{
    def assertStackTag(stack:ItemStack)
    {
        if (!stack.hasTagCompound) stack.setTagCompound(new NBTTagCompound)
    }

    def isValidChip(stack:ItemStack) =
    {
        stack != null && stack.getItem.isInstanceOf[ItemRoutingChip] &&
            RoutingChipDefs.getForStack(stack) != null
    }

    def hasChipInside(stack:ItemStack) =
    {
        isValidChip(stack) && stack.hasTagCompound && stack.getTagCompound.hasKey("chipROM")
    }

    def saveChipToItemStack(stack:ItemStack, chipset:RoutingChip)
    {
        assertStackTag(stack)
        val tag1 = stack.getTagCompound
        val tag2 = new NBTTagCompound
        chipset.save(tag2)
        tag1.setTag("chipROM", tag2)
    }

    def loadChipFromItemStack(stack:ItemStack) =
    {
        val e = RoutingChipDefs.getForStack(stack)
        val chip = e.createChipset
        if (stack.hasTagCompound && stack.getTagCompound.hasKey("chipROM"))
            chip.load(stack.getTagCompound.getCompoundTag("chipROM"))
        chip
    }
}

object RoutingChipDefs extends ItemDefinition
{
    type EnumVal = ChipVal
    override def getItem = ProjectRedTransportation.itemRoutingChip

    val ITEMRESPONDER = new ChipVal("responder", new ChipItemResponder)
    val DYNAMICITEMRESPONDER = new ChipVal("dyn_responder", new ChipDynamicItemResponder)
    val ITEMOVERFLOWRESPONDER = new ChipVal("overflow", new ChipItemOverflowResponder)
    val ITEMTERMINATOR = new ChipVal("terminator", new ChipItemTerminator)
    val ITEMEXTRACTOR = new ChipVal("extractor", new ChipExtractor)
    val ITEMBROADCASTER = new ChipVal("broadcaster", new ChipBroadcaster)
    val ITEMSTOCKKEEPER = new ChipVal("stock_keeper", new ChipStockKeeper)
    val ITEMCRAFTING = new ChipVal("crafting", new ChipCrafting, ChipType.CRAFTING)
    val ITEMEXTENSION = new ChipVal("extension", new ChipCraftingExtension)

    def getForStack(stack:ItemStack) =
    {
        if (stack != null && stack.getItem.isInstanceOf[ItemRoutingChip] &&
            values.isDefinedAt(stack.getItemDamage)) values(stack.getItemDamage)
        else null
    }

    class ChipVal(iconPath:String, f: => RoutingChip, cType:ChipType) extends ItemDef(iconPath)
    {
        def this(icon:String, f: => RoutingChip) = this(icon, f, ChipType.INTERFACE)

        var icon:TextureAtlasSprite = _

        def registerIcons(map:TextureMap)
        {
            icon = map.registerSprite(new ResourceLocation("projectred:items/mechanical/"+iconPath))
        }

        def setCustomModelResourceLocations()
        {
            ModelLoader.setCustomModelResourceLocation(getItem, meta, new ModelResourceLocation("projectred:mechanical/"+iconPath))
        }

        def isInterfaceChip = cType == ChipType.INTERFACE
        def isCraftingChip = cType == ChipType.CRAFTING

        def createChipset = f
    }
}

object ChipType extends Enumeration
{
    type ChipType = Value
    val INTERFACE, CRAFTING = Value
}

class ItemRouterUtility extends ItemCore
{
    setMaxStackSize(1)
    setCreativeTab(ProjectRedTransportation.tabTransportation)
}
