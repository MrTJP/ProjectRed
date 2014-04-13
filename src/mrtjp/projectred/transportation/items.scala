package mrtjp.projectred.transportation

import codechicken.lib.render.{CCRenderState, TextureUtils}
import codechicken.lib.vec.{Translation, Scale, BlockCoord, Vector3}
import codechicken.multipart.{TItemMultiPart, MultiPartRegistry}
import cpw.mods.fml.relauncher.{SideOnly, Side}
import java.util.{List => JList}
import mrtjp.projectred.ProjectRedTransportation
import net.minecraft.block.Block
import net.minecraft.client.renderer.texture.IconRegister
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.{Icon, EnumChatFormatting}
import net.minecraft.world.World
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.{ItemRendererHelper, ItemRenderType}
import org.lwjgl.input.Keyboard
import scala.collection.mutable.ListBuffer
import mrtjp.projectred.transportation.EnumRoutingChip.ChipType.ChipType
import scala.collection.mutable

class ItemPartPipe(id:Int) extends Item(id) with TItemMultiPart
{
    setHasSubtypes(true)
    setCreativeTab(ProjectRedTransportation.tabTransportation)
    setUnlocalizedName("projectred.transportation.pipe")

    def newPart(item:ItemStack, player:EntityPlayer, world:World, pos:BlockCoord, side:Int, vhit:Vector3) =
    {
        val pdef = PipeDef.VALID_PIPE(item.getItemDamage)
        val p = MultiPartRegistry.createPart(pdef.partname, false).asInstanceOf[FlowingPipePart]
        if (p != null) p.preparePlacement(side, item.getItemDamage)
        p
    }

    override def onItemUse(stack:ItemStack, player:EntityPlayer, w:World, x:Int, y:Int, z:Int, side:Int, hitX:Float, hitY:Float, hitZ:Float) =
    {
        if (super.onItemUse(stack, player, w, x, y, z, side, hitX, hitY, hitZ))
        {
            w.playSoundEffect(x+0.5, y+0.5, z+0.5, Block.soundGlassFootstep.getPlaceSound,
                Block.soundGlassFootstep.getVolume*5.0F, Block.soundGlassFootstep.getPitch*0.9F)
            true
        }
        else false
    }

    override def getUnlocalizedName(stack:ItemStack) = super.getUnlocalizedName+"|"+stack.getItemDamage

    @SideOnly(Side.CLIENT)
    override def getSubItems(id:Int, tab:CreativeTabs, list:JList[_])
    {
        val l2 = list.asInstanceOf[JList[ItemStack]]
        for (t <- PipeDef.VALID_PIPE) l2.add(t.getItemStack)
    }

    override def registerIcons(reg:IconRegister)
    {
        for (p <- PipeDef.VALID_PIPE) p.loadTextures(reg)
    }

    @SideOnly(Side.CLIENT)
    override def getSpriteNumber = 0
}

object PipeItemRenderer extends IItemRenderer
{
    def handleRenderType(item:ItemStack, r:ItemRenderType) = true
    def shouldUseRenderHelper(r:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true

    def renderItem(rtype:ItemRenderType, item:ItemStack, data:AnyRef*)
    {
        val damage = item.getItemDamage
        import ItemRenderType._
        rtype match
        {
            case ENTITY => renderWireInventory(damage, -.5f, 0f, -.5f, 1f)
            case EQUIPPED => renderWireInventory(damage, 0f, .0f, 0f, 1f)
            case EQUIPPED_FIRST_PERSON => renderWireInventory(damage, 1f, -.6f, -.4f, 2f)
            case INVENTORY => renderWireInventory(damage, 0f, -.1f, 0f, 1f)
            case _ =>
        }
    }

    def renderWireInventory(meta:Int, x:Float, y:Float, z:Float, scale:Float)
    {
        val pdef:PipeDef = PipeDef.VALID_PIPE(meta)
        if (pdef == null) return
        TextureUtils.bindAtlas(0)
        CCRenderState.reset()
        CCRenderState.useNormals(true)
        CCRenderState.pullLightmap()
        CCRenderState.setColourOpaque(-1)
        CCRenderState.startDrawing(7)

        RenderPipe.renderInv(new Scale(scale).`with`(new Translation(x, y, z)), pdef.sprites(0))

        CCRenderState.draw()
    }
}

class ItemRoutingChip(id:Int) extends Item(id)
{
    setUnlocalizedName("projectred.transportation.routingchip")
    setCreativeTab(ProjectRedTransportation.tabTransportation)
    setHasSubtypes(true)

    override def getSubItems(id:Int, tab:CreativeTabs, list:JList[_])
    {
        val list2 = list.asInstanceOf[JList[ItemStack]]
        for (c <- EnumRoutingChip.VALID_CHIPS) list2.add(c.getItemStack)
    }

    override def getUnlocalizedName(stack:ItemStack) = super.getUnlocalizedName+"|"+stack.getItemDamage

    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IconRegister)
    {
        for (c <- EnumRoutingChip.VALID_CHIPS) c.registerIcons(reg)
    }

    override def getIconFromDamage(meta:Int) =
    {
        val c = EnumRoutingChip.get(meta)
        if (c != null) c.icon
        else null
    }

    override def addInformation(stack:ItemStack, player:EntityPlayer, list:JList[_], par4:Boolean)
    {
        val list2 = list.asInstanceOf[JList[String]]
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT)) if (stack.hasTagCompound)
        {
            import scala.collection.JavaConversions._
            val r = ItemRoutingChip.loadChipFromItemStack(stack)
            if (r != null)
            {
                val s = new ListBuffer[String]
                r.infoCollection(s)
                list2.addAll(s)
            }
        }
        else list2.add(EnumChatFormatting.GRAY+"not configured")
    }

    override def onItemRightClick(stack:ItemStack, w:World, player:EntityPlayer):ItemStack =
    {
        if (!w.isRemote && stack != null && stack.getItem.isInstanceOf[ItemRoutingChip])
        {
            val r = ItemRoutingChip.loadChipFromItemStack(stack)
            if (r != null) r.openGui(player)
        }
        super.onItemRightClick(stack, w, player)
    }

    override def onItemUse(stack:ItemStack, player:EntityPlayer, w:World, par4:Int, par5:Int, par6:Int, par7:Int, par8:Float, par9:Float, par10:Float):Boolean =
    {
        if (!w.isRemote && stack != null && stack.getItem.isInstanceOf[ItemRoutingChip])
        {
            val r = ItemRoutingChip.loadChipFromItemStack(stack)
            if (r != null) r.openGui(player)
        }
        true
    }

    override def shouldPassSneakingClickToBlock(par2World:World, par4:Int, par5:Int, par6:Int) = true
}

object ItemRoutingChip
{
    def saveChipToItemStack(stack:ItemStack, chipset:RoutingChipset)
    {
        if (stack == null || chipset == null || !stack.getItem.isInstanceOf[ItemRoutingChip]) return
        val mainTag = new NBTTagCompound("main")
        val chipTag = new NBTTagCompound("ROM")
        chipset.save(chipTag)
        mainTag.setTag("chipROM", chipTag)
        stack.setTagCompound(mainTag)
    }

    def loadChipFromItemStack(stack:ItemStack) =
    {
        if (stack == null || !stack.getItem.isInstanceOf[ItemRoutingChip]) null
        else
        {
            val e = EnumRoutingChip.get(stack.getItemDamage)
            if (e != null)
            {
                val chip = e.createChipset
                val mainTag = stack.getTagCompound
                if (mainTag != null && mainTag.hasKey("chipROM")) chip.load(mainTag.getCompoundTag("chipROM"))
                chip
            }
            else null
        }
    }
}

object EnumRoutingChip extends Enumeration
{
    type EnumRoutingChip = ChipVal

    val ITEMRESPONDER = new ChipVal("responder", new ChipItemResponder)
    val DYNAMICITEMRESPONDER = new ChipVal("responder_dyn", new ChipDynamicItemResponder)
    val ITEMOVERFLOWRESPONDER = new ChipVal("overflow", new ChipItemOverflowResponder)
    val ITEMTERMINATOR = new ChipVal("terminator", new ChipItemTerminator)
    val ITEMEXTRACTOR = new ChipVal("extractor", new ChipExtractor)
    val ITEMBROADCASTER = new ChipVal("broadcaster", new ChipBroadcaster)
    val ITEMSTOCKKEEPER = new ChipVal("stockkeeper", new ChipStockKeeper)
    val ITEMCRAFTING = new ChipVal("crafting", new ChipCrafting, ChipType.CRAFTING)

    val VALID_CHIPS =
    {
        var array = new mutable.ArrayBuilder.ofRef[EnumRoutingChip]()
        for (v <- values) array += v.asInstanceOf[EnumRoutingChip]
        array.result()
    }

    def getForStack(stack:ItemStack):EnumRoutingChip =
    {
        if (stack != null && stack.getItem.isInstanceOf[ItemRoutingChip]) return get(stack.getItemDamage)
        null
    }

    def get(ordinal:Int):EnumRoutingChip =
    {
        if (ordinal < 0 || ordinal >= VALID_CHIPS.length) null
        else VALID_CHIPS(ordinal)
    }

    class ChipVal(iconPath:String, f: => RoutingChipset, cType:ChipType) extends Val
    {
        def this(icon:String, f: => RoutingChipset) = this(icon, f, ChipType.INTERFACE)
        val meta = id
        var icon:Icon = null

        def registerIcons(reg:IconRegister)
        {
            icon = reg.registerIcon("projectred:chips/"+iconPath)
        }

        def getItemStack:ItemStack = getItemStack(1)

        def getItemStack(i:Int) = new ItemStack(ProjectRedTransportation.itemRoutingChip, i, meta)

        def isInterfaceChip = cType == ChipType.INTERFACE

        def isCraftingChip = cType == ChipType.CRAFTING

        def createChipset = f
    }

    object ChipType extends Enumeration
    {
        type ChipType = Value
        val INTERFACE, CRAFTING = Value
    }
}