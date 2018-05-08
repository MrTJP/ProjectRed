/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import java.util
import java.util.function.Supplier
import java.util.{Collections, ArrayList => JAList}

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.model.bakedmodels.WrappedItemModel
import codechicken.lib.model.bakery.{CCBakeryModel, SimpleBlockRenderer}
import codechicken.lib.render.buffer.BakingVertexBuffer
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.render.item.entity.WrappedEntityItemRenderer
import codechicken.lib.render.{CCModel, CCRenderState, OBJParser}
import codechicken.lib.texture.TextureUtils
import codechicken.lib.util.{TransformUtils, VertexDataUtils}
import codechicken.lib.vec._
import codechicken.lib.vec.uv.{IconTransformation, MultiIconTransformation, UVTransformation}
import com.google.common.collect.ImmutableList
import com.mojang.realmsclient.gui.ChatFormatting.{BOLD, RED, RESET}
import mrtjp.core.gui._
import mrtjp.core.inventory.{InvWrapper, TInventory, TInventoryCapablilityTile}
import mrtjp.core.item.{ItemKey, ItemKeyStack}
import mrtjp.core.vec.{Point, Rect, Size, Vec2}
import mrtjp.projectred.ProjectRedCore.log
import mrtjp.projectred.core.PartDefs
import mrtjp.projectred.integration.ComponentStore
import mrtjp.projectred.transmission.WireDef
import net.minecraft.block.state.{BlockFaceShape, IBlockState}
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.GlStateManager._
import net.minecraft.client.renderer.block.model._
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.entity.EntityLivingBase
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.{CraftingManager, ShapedRecipes, ShapelessRecipes}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraft.world.{IBlockAccess, World}
import net.minecraftforge.common.model.IModelState
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import net.minecraftforge.oredict.{ShapedOreRecipe, ShapelessOreRecipe}
import org.lwjgl.opengl.GL11._

import scala.collection.JavaConversions._
import scala.collection.mutable.{Map => MMap, Set => MSet}

class TileICPrinter extends TileICMachine with TInventory with TInventoryCapablilityTile
{
    var progress = 0.0
    var speed = 0.0
    var isWorking = false
    var inputICState = 0 // 0 - none, 1 - blank, 2 - written

    var externalItems = Set[ItemKey]()
    var watchers = MSet[EntityPlayer]()

    var requirementsDirty = true
    var requirements = Seq[ItemKeyStack]()

    override def getDisplayName = super.getDisplayName

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setFloat("prog", progress.toFloat)
        tag.setFloat("sp", speed.toFloat)
        tag.setBoolean("w", isWorking)
        tag.setByte("in", inputICState.toByte)
        saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        progress = tag.getFloat("prog")
        speed = tag.getFloat("sp")
        isWorking = tag.getBoolean("w")
        inputICState = tag.getByte("in")
        loadInv(tag)
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeFloat(progress.toFloat)
        out.writeFloat(speed.toFloat)
        out.writeBoolean(isWorking)
        out.writeByte(inputICState)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        progress = in.readFloat()
        speed = in.readFloat()
        isWorking = in.readBoolean()
        inputICState = in.readByte()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 4 => isWorking = false
        case 5 =>
            isWorking = true
            progress = in.readFloat()
            speed = in.readFloat()
        case 6 => inputICState = in.readByte()
        case 7 =>
            externalItems = Set.empty
            for (i <- 0 until in.readInt())
                externalItems += ItemKey.get(in.readItemStack())
        case _ => super.read(in, key)
    }

    def sendStartWorking()
    {
        val out = writeStream(5)
        out.writeFloat(progress.toFloat)
        out.writeFloat(speed.toFloat)
        out.sendToChunk(this)
    }

    def sendStopWorking()
    {
        writeStream(4).sendToChunk(this)
    }

    def sendInputICStateUpdate()
    {
        writeStream(6).writeByte(inputICState).sendToChunk(this)
    }

    def sendExternalItemMap(players:Iterable[EntityPlayer])
    {
        val out = writeStream(7).writeInt(externalItems.size)
        for (item <- externalItems)
            out.writeItemStack(item.makeStack(0))
        out.sendToChunk(this)
    }

    //0 - 17 = ingredients
    //18 = Blueprint input
    //19 = IC Input
    //20 = Output
    override def isItemValidForSlot(slot:Int, item:ItemStack) =
    {
        if (slot == 18) item.getItem.isInstanceOf[ItemICBlueprint] && ItemICBlueprint.hasICInside(item)
        else if (slot == 19) item.getItem.isInstanceOf[ItemICChip]
        else if (slot == 20) false
        else true
    }

    override protected val storage = Array.fill(21)(ItemStack.EMPTY)//new Array[ItemStack](21)
    override def getInventoryStackLimit = 64
    override def getName = "icprinter"

    override def getBlockFaceShape(side:Int) = BlockFaceShape.UNDEFINED//TODO, Do the do with the thing and the do.

    override def updateServer()
    {
        if (isWorking) {
            if (world.getTotalWorldTime%10 == 0 && !canStart) {
                doStop()
            } else {
                progress += speed
                if (progress >= 1.0) {
                    doStop()
                    if (canStart) onFinished()
                }
            }
        }

        if (!isWorking && world.getTotalWorldTime%10 == 0 && canStart) //delay check, can be expensive
            doStart()
    }

    def canStart =
    {
        if (world.getTotalWorldTime%20 == 0)
            checkIngredients() && checkOutputClear && checkInputIC && checkBlueprint && checkBlueprintFlags
        else checkOutputClear && checkInputIC && checkBlueprint && checkBlueprintFlags && checkIngredients() //use cheaper checks as fail-fast
    }

    def checkBlueprint =
    {
        val stack = getStackInSlot(18)
        !stack.isEmpty && stack.getItem.isInstanceOf[ItemICBlueprint] &&
                ItemICBlueprint.hasICInside(stack)
    }

    def checkBlueprintFlags =
    {
        val (_, err) = ItemICBlueprint.loadFlags(getStackInSlot(18))
        err == 0
    }

    private def checkInputIC =
    {
        val stack = getStackInSlot(19)
        !stack.isEmpty && stack.getItem.isInstanceOf[ItemICChip]
    }

    private def checkOutputClear = getStackInSlot(20).isEmpty

    private def checkIngredients():Boolean =
    {
        val ic = getStackInSlot(19)
        if (!ic.isEmpty && ic.getItemDamage == 1) return true

        val oldMap = externalItems
        val hasEnough = getRequiredResources.forall(containsEnoughOf)
        if (externalItems != oldMap) sendExternalItemMap(watchers)
        hasEnough
    }

    def getRequiredResources =
    {
        if (requirementsDirty)
        {
            val stack = getStackInSlot(18)
            requirements = if (!stack.isEmpty && ItemICBlueprint.hasICInside(stack))
            {
                val ic = ItemICBlueprint.loadTileMap(stack)
                TileICPrinter.resolveResources(ic)
            }
            else Seq()
            requirementsDirty = false
        }
        requirements
    }

    def containsEnoughOf(stack:ItemKeyStack):Boolean =
    {
        var a = 0
        for (i <- 0 until 18)
        {
            val s = getStackInSlot(i)
            if (!s.isEmpty && ItemKey.get(s) == stack.key)
            {
                a += s.getCount
                if (a >= stack.stackSize) return true
            }
        }

        if (!world.isRemote)
        {
            externalItems -= stack.key
            for (s <- 0 until 6 if s != 1)
            {
                val inv = InvWrapper.getInventory(world, pos.offset(EnumFacing.values()(s)))
                if (inv != null)
                {
                    val w = InvWrapper.wrapInternal(inv).setSlotsFromSide(s^1)
                    val in = w.getItemCount(stack.key)
                    if (in > 0)
                    {
                        a += in
                        if (a >= stack.stackSize)
                        {
                            externalItems += stack.key
                            return true
                        }
                    }
                }
            }
        }
        else return externalItems.contains(stack.key)

        false
    }

    def eatResource(stack:ItemKeyStack)
    {
        var left = stack.stackSize
        for (i <- 0 until 18)
        {
            val s = getStackInSlot(i)
            if (!s.isEmpty && stack.key == ItemKey.get(s))
            {
                val toEat = math.min(left, s.getCount)
                left -= toEat
                s.shrink(toEat)
                if (s.getCount <= 0) setInventorySlotContents(i, ItemStack.EMPTY)
                else setInventorySlotContents(i, s)
                if (left <= 0) return
            }
        }

        for (s <- 0 until 6 if s != 1)
        {
            val inv = InvWrapper.getInventory(world, pos.offset(EnumFacing.values()(s)))
            if (inv != null)
            {
                val w = InvWrapper.wrapInternal(inv).setSlotsFromSide(s^1)
                left -= w.extractItem(stack.key, left)
                if (left <= 0) return
            }
        }
    }

    def doStart()
    {
        isWorking = true
        progress = 0.0
        speed = if (getStackInSlot(19).getItemDamage == 1) 0.05 else 0.0005
        sendStartWorking()
    }

    def doStop()
    {
        isWorking = false
        progress = 0.0
        speed = 0.0
        sendStopWorking()
    }

    def onFinished()
    {

        val bp = getStackInSlot(18)
        val chip = getStackInSlot(19)

        if (chip.getItemDamage != 1) getRequiredResources.foreach(eatResource)

        ItemICBlueprint.copyIC(bp, chip)
        setInventorySlotContents(19, ItemStack.EMPTY)
        setInventorySlotContents(20, chip)
    }

    override def markDirty()
    {
        super.markDirty()
        requirementsDirty = true
        if (!world.isRemote)
        {
            if (!canStart && isWorking) doStop()
            val oldICState = inputICState

            if (!getStackInSlot(20).isEmpty) inputICState = 2
            else
            {
                val s = getStackInSlot(19)
                if (!s.isEmpty)
                {
                    if (ItemICBlueprint.hasICInside(s)) inputICState = 2
                    else inputICState = 1
                }
                else inputICState = 0
            }

            if (inputICState != oldICState) sendInputICStateUpdate()
        }
    }

    override def onBlockActivated(player:EntityPlayer, side:Int):Boolean =
    {
        if (super.onBlockActivated(player, side)) return true
        if (!world.isRemote)
            GuiICPrinter.open(player, createContainer(player), _.writePos(pos))
        true
    }

    def createContainer(player:EntityPlayer) =
    {
        val c = new ContainerPrinter(player, this)
        c.startWatchDelegate = {p =>
            watchers += p
            sendExternalItemMap(MSet(p))
        }
        c.stopWatchDelegate = {watchers -= _}
        c
    }

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(world, pos)
    }

    //Client-side render things
    import TileICPrinter._
    var lProgress = 1.0
    var lSpeed = 0.0
    var lState = LERPTOREST
    private var lerpCount = 0

    override def updateClient()
    {
        if (isWorking) {
            progress += speed
            progress = math.min(progress, 1.0)
        }

        lState match {
            case REST =>
                lProgress = 0
                lSpeed = 0
                if (isWorking) lState = `REALTIME`
            case LERPTOREST =>
                lSpeed = -0.025
                lProgress += lSpeed
                if (lProgress <= 0) lState = REST
                else if (isWorking) lState = LERPTOREALTIME
            case LERPTOREALTIME =>
                if (lProgress > progress) {
                    lSpeed = -0.16
                    lProgress += lSpeed
                    if (lProgress <= progress) lState = REALTIME
                } else {
                    lSpeed = 0.16
                    lProgress += lSpeed
                    if (lProgress >= progress) lState = REALTIME
                }
            case REALTIME =>
                if (progress >= 1.0 || !isWorking) {
                    lerpCount = 0
                    lState = FIN
                } else {
                    lProgress = progress
                    lSpeed = speed
                }
            case FIN =>
                lSpeed = 0
                lerpCount += 1
                if (lerpCount >= 25) lState = LERPTOREST
        }
    }
}

object TileICPrinter
{
    //render states
    val REST = 0
    val LERPTOREST = 1
    val LERPTOREALTIME = 2
    val REALTIME = 3
    val FIN = 4

    private var gRec = Map[(ItemKey), Seq[ItemKey]]()

    def cacheRecipe(key:ItemKey)
    {
        val recipes = CraftingManager.REGISTRY.iterator
        for (r <- recipes) try
        {
            val out = ItemKey.get(r.getRecipeOutput)
            if (out == key)
            {
                //TODO, We need to do proper ingredient matching.
                val inputs = r.getIngredients.map(_.getMatchingStacks).filterNot(_ == null).map(i => ItemKey.get(i.head))
//                    r match
//                {
//                    case s:ShapedRecipes => s.recipeItems.toSeq.filterNot(_.getMatchingStacks.isEmpty).map(ItemKey.get)
//
//                    case s:ShapelessRecipes => s.recipeItems.toSeq.filterNot(_.getMatchingStacks.isEmpty).map(ItemKey.get)
//
//                    case s:ShapedOreRecipe => s.getInput.toSeq.flatMap {
//                        case s:ItemStack => Seq(s)
//                        case a:JAList[_] => a.toSeq.asInstanceOf[Seq[ItemStack]]
//                        case _ => Seq.empty
//                    }.map(ItemKey.get)
//
//                    case s:ShapelessOreRecipe => s.getInput.toSeq.flatMap {
//                        case s:ItemStack => Seq(s)
//                        case a:JAList[_] => a.toSeq.asInstanceOf[Seq[ItemStack]]
//                        case _ => Seq.empty
//                    }.map(ItemKey.get)
//
//                    case _ => Seq.empty
//                }
                if (inputs.nonEmpty)
                {
                    gRec += key -> inputs
                    return
                }
            }
        }
        catch {
            case e:Exception =>
                log.error(s"Some mod messed up. The recipe $r has a null output.")
        }
        gRec += key -> Seq.empty
    }

    def getOrCacheComponents(in:ItemStack):Seq[ItemKey] =
    {
        val key = ItemKey.get(in)
        if (!gRec.contains(key)) cacheRecipe(key)
        gRec(key)
    }

    def resolveResources(tmap:ICTileMapContainer) =
    {
        val map = MMap[ItemKey, Double]()

        import mrtjp.core.item.ItemKeyConversions._
        def add(key:ItemKey, amount:Double)
        {
            val c = map.getOrElse(key, 0.0)
            map(key) = c+amount
        }

        def addComponents(stack:ItemStack)
        {
            getOrCacheComponents(stack).foreach(add(_, 0.25))
        }

        import mrtjp.projectred.fabrication.{ICGateDefinition => gd}

        for (part <- tmap.tiles.values) part match
        {
//            case p:TorchICPart => add(new ItemStack(Blocks.REDSTONE_TORCH), 0.25)
            case p:LeverICTile => add(new ItemStack(Blocks.LEVER), 0.25)
            case p:ButtonICTile => add(new ItemStack(Blocks.STONE_BUTTON), 0.25)
            case p:AlloyWireICTile => add(WireDef.RED_ALLOY.makeStack, 0.25)
            case p:InsulatedWireICTile => add(WireDef.INSULATED_WIRES(p.colour&0xFF).makeStack, 0.25)
            case p:BundledCableICTile => add(WireDef.BUNDLED_WIRES((p.colour+1)&0xFF).makeStack, 0.25)
            case p:GateICTile => gd.apply(p.subID) match
            {
                case gd.IOSimple =>
                    add(new ItemStack(Items.GOLD_NUGGET), 1)
                    add(PartDefs.PLATE.makeStack, 1.50)
                    add(PartDefs.CONDUCTIVEPLATE.makeStack, 0.50)
                case gd.IOAnalog =>
                    add(new ItemStack(Items.GOLD_NUGGET), 1)
                    add(PartDefs.PLATE.makeStack, 1.50)
                    add(new ItemStack(Items.REDSTONE), 0.25)
                    add(PartDefs.CONDUCTIVEPLATE.makeStack, 0.25)
                case gd.IOBundled =>
                    add(new ItemStack(Items.GOLD_NUGGET), 1)
                    add(PartDefs.PLATE.makeStack, 1.50)
                    add(PartDefs.BUNDLEDPLATE.makeStack, 0.25)
                    add(PartDefs.CONDUCTIVEPLATE.makeStack, 0.25)
                case d if d.intDef != null => addComponents(d.intDef.makeStack)
                case _ =>
            }
            case _ =>
        }

        map.map(e => ItemKeyStack.get(e._1, e._2.ceil.toInt)).toSeq.sorted
    }
}

class ContainerPrinter(player:EntityPlayer, tile:TileICPrinter) extends NodeContainer
{
    {
        var i = 0
        for ((x, y) <- GuiLib.createSlotGrid(8, 75, 9, 2, 0, 0))
        {
            addSlotToContainer(new Slot3(tile, i, x, y))
            i += 1
        }
        addSlotToContainer({val s = new Slot3(tile, 18, 63, 20); s.slotLimitCalculator = {() => 1}; s})
        addSlotToContainer({val s = new Slot3(tile, 19, 63, 46); s.slotLimitCalculator = {() => 1}; s})
        addSlotToContainer({val s = new Slot3(tile, 20, 134, 33); s.slotLimitCalculator = {() => 1}; s})

        addPlayerInv(player, 8, 119)
    }

    //0 - 17 = ingredients
    //18 = Blueprint input
    //19 = IC Input
    //20 = Output
    override def doMerge(stack:ItemStack, from:Int):Boolean =
    {
        if (from == 20)
        {
            if (tryMergeItemStack(stack, 21, 57, true)) return true
        }
        else if (0 until 20 contains from)
        {
            if (tryMergeItemStack(stack, 21, 57, false)) return true
        }
        else
        {
            stack.getItem match
            {
                case i:ItemICBlueprint => if (tryMergeItemStack(stack, 18, 19, false)) return true
                case i:ItemICChip => if (tryMergeItemStack(stack, 19, 20, false)) return true
                case _ =>
            }

            if (tryMergeItemStack(stack, 0, 18, false)) return true
        }
        false
    }
}

class GuiICPrinter(c:ContainerPrinter, tile:TileICPrinter) extends NodeGui(c, 176, 201)
{
    var list:ItemListNode = null
    val flagBox = new Rect(86, 19, 8, 8)

    var hasErrors = false

    {
        val clip = new ClipNode
        clip.position = Point(8, 17)
        clip.size = Size(48, 48)
        addChild(clip)

        val pan = new PanNode
        pan.size = Size(48, 48)
        pan.scrollModifier = Vec2(0, 1)
        pan.scrollBarHorizontal = false
        clip.addChild(pan)

        list = new ItemListNode
        list.zPosition = -0.01
        list.itemSize = Size(14, 14)
        list.displayNodeFactory = {stack =>
            val d = new ItemDisplayNode
            d.zPosition = -0.01
            d.backgroundColour = if (tile.containsEnoughOf(stack))
                EnumColour.LIME.argb(0x44) else EnumColour.RED.argb(0x44)
            d
        }
        pan.addChild(list)
        list.items = tile.getRequiredResources
        list.reset()
    }

    override def update_Impl()
    {
        if (mcInst.world.getTotalWorldTime%10 == 0) {
            list.items = tile.getRequiredResources
            list.reset()
        }

        hasErrors = tile.checkBlueprint && !tile.checkBlueprintFlags
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        TextureUtils.changeTexture(GuiICPrinter.background)
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, 176, 201)
        if (tile.isWorking) {
            val dx = 37*tile.progress
            GuiDraw.drawTexturedModalRect(86, 32, 176, 0, dx.toInt, 18)
        }
        if (hasErrors)
            GuiDraw.drawTexturedModalRect(flagBox.x, flagBox.y, 176, 19, flagBox.width, flagBox.height) //draw the error symbol

        GuiDraw.drawString("IC Printer", 8, 6, EnumColour.GRAY.argb, false)
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        if (hasErrors) {
            val m2 = convertPointFromScreen(mouse)
            if (flagBox.contains(m2))
                GuiDraw.drawMultiLineTip(ItemStack.EMPTY, m2.x+12, m2.y-12,
                    Seq(s"$RED$BOLD" + "X" + s"$RESET blueprint contains errors"))
        }
    }
}

object GuiICPrinter extends TGuiFactory
{
    val background = new ResourceLocation("projectred", "textures/gui/ic_printer.png")

    override def getID = FabricationProxy.icPrinterGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        player.world.getTileEntity(data.readPos()) match {
            case t:TileICPrinter => new GuiICPrinter(t.createContainer(player), t)
            case _ => null
        }
    }
}

object RenderICPrinter extends SimpleBlockRenderer
{
    import java.lang.{Integer => JInt}
    import java.util.{List => JList}

    import BlockICMachine._
    import org.apache.commons.lang3.tuple.Triple

    val lowerBoxes =
    {
        val array = new Array[CCModel](4)
        val box = CCModel.quadModel(24).generateBlock(0, new Cuboid6(0, 0, 0, 1, 10/16D, 1))
        for (r <- 0 until 4) {
            val m = box.copy.apply(Rotation.quarterRotations(r).at(Vector3.center))
            m.computeNormals()
            m.shrinkUVs(0.0005)
            array(r) = m
        }
        array
    }

    var headIcon:TextureAtlasSprite = _
    var bottom:TextureAtlasSprite = _
    var side1:TextureAtlasSprite = _
    var side2:TextureAtlasSprite = _
    var top:TextureAtlasSprite = _

    var iconT:UVTransformation = _

    override def handleState(state:IExtendedBlockState, world: IBlockAccess, pos: BlockPos):IExtendedBlockState = world.getTileEntity(pos) match {
        case t:TileICPrinter =>
            state.withProperty(UNLISTED_ROTATION_PROPERTY, t.rotation.asInstanceOf[JInt])
        case _ => state
    }

    override def getWorldTransforms(state:IExtendedBlockState) =
    {
        val rot = state.getValue(UNLISTED_ROTATION_PROPERTY)
        Triple.of(0, rot, iconT)
    }

    override def getItemTransforms(stack:ItemStack) = Triple.of(0, 0, iconT)

    override def shouldCull() = false

    override def bakeQuads(face:EnumFacing, state:IExtendedBlockState):JList[BakedQuad] =
    {
        val buffer = BakingVertexBuffer.create
        val worldData = getWorldTransforms(state)
        val ccrs = CCRenderState.instance

        ccrs.reset()
        ccrs.startDrawing(0x7, DefaultVertexFormats.ITEM, buffer)
        lowerBoxes(worldData.getMiddle).render(ccrs, worldData.getRight)
        buffer.finishDrawing()

        val quads = buffer.bake
        if (face == null && !shouldCull)
            return quads
        else if (face != null)
            return VertexDataUtils.sortFaceData(quads).get(face)

        ImmutableList.of()
    }

    override def bakeItemQuads(face:EnumFacing, stack:ItemStack):JList[BakedQuad] =
    {
        val buffer = BakingVertexBuffer.create
        val worldData = getItemTransforms(stack)
        val ccrs = CCRenderState.instance

        ccrs.reset()
        ccrs.startDrawing(0x7, DefaultVertexFormats.ITEM, buffer)
        lowerBoxes(worldData.getMiddle).render(ccrs, worldData.getRight)
        buffer.finishDrawing()

        val quads = buffer.bake
        if (face == null && !shouldCull)
            return quads
        else if (face != null)
            return VertexDataUtils.sortFaceData(quads).get(face)
        ImmutableList.of()
    }

    override def registerIcons(reg:TextureMap)
    {
        def register(t:String) = reg.registerSprite(new ResourceLocation("projectred:blocks/fabrication/printer/"+t))

        headIcon = register("printerhead")
        bottom = register("bottom")
        side1 = register("side1")
        side2 = register("side2")
        top = register("top")

        iconT = new MultiIconTransformation(bottom, top, side2, side1, side2, side2)
    }
}

object RenderICPrinterDynamic extends TileEntitySpecialRenderer[TileICPrinter]
{
    var models = getMods

    def getMods =
    {
        val map = OBJParser.parseModels(new ResourceLocation("projectred:textures/obj/fabrication/printer.obj"), 7, null).toMap
        map.values.foreach { m =>
            m.verts = m.backfacedCopy.verts
            m.apply(new Translation(8/16D, 10/16D, 8/16D))
            m.computeNormals()
            m.shrinkUVs(0.0005)
        }
        map
    }

    var progress = 0.0
    var speed = 0.0
    var frame = 0.0
    var icState = 0
    var rasterMode = false

    override def render(tile:TileICPrinter, x:Double, y:Double, z:Double, partialTicks:Float, destroyStage:Int, alpha:Float)
    {
        val ptile = tile.asInstanceOf[TileICPrinter]

        progress = ptile.lProgress
        speed = ptile.lSpeed
        frame = partialTicks
        icState = ptile.inputICState
        rasterMode = ptile.lState == TileICPrinter.REALTIME

        val t = ptile.rotationT `with` new Translation(x, y, z)
        val iconT = new MultiIconTransformation(RenderICPrinter.headIcon)
        val ccrs = CCRenderState.instance()
        TextureUtils.bindBlockTexture()

        enableBlend()
        blendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

        ccrs.reset()
        ccrs.pullLightmap()
        ccrs.startDrawing(0x7, DefaultVertexFormats.ITEM)
        renderPrinter(ccrs, t, iconT)
        ccrs.draw()

        disableBlend()
    }

    def renderPrinter(ccrs:CCRenderState, t:Transformation, iconT:UVTransformation)
    {
        if (icState != 0) renderICChip(ccrs, t)
        renderFrame(ccrs, t, iconT)
        renderShaft(ccrs, t, iconT)
        renderGlass(ccrs, t, iconT)
    }

    def renderICChip(ccrs:CCRenderState, t:Transformation)
    {
        import ComponentStore._
        icChip.render(ccrs, Rotation.quarterRotations(2) `with` new Translation(0.5, 9.5/16D, 0.5) `with` t,
            new IconTransformation(if (icState == 1) icChipIconOff else icChipIcon))
    }

    def renderFrame(ccrs:CCRenderState, t:Transformation, iconT:UVTransformation)
    {
        models("frame").render(ccrs, t, iconT)
    }

    def renderShaft(ccrs:CCRenderState, t:Transformation, iconT:UVTransformation)
    {
        val min = -4.5/16D
        val max = 4.5/16D
        val p = progress+speed*frame
        val subT = new Translation(0, 0, min+(max-min)*p) `with` t

        models("shaft").render(ccrs, subT, iconT)
        renderHead(ccrs, subT, iconT)
    }

    def renderHead(ccrs:CCRenderState, t:Transformation, iconT:UVTransformation)
    {
        val amp = 3.5/16D
        val freq = 900
        val p = progress+speed*frame
        val trans = if (rasterMode) math.cos(p*freq)*amp else amp*2*p-amp
        val subT = new Translation(trans, 0, 0) `with` t
        models("head").render(ccrs, subT, iconT)
    }

    def renderGlass(ccrs:CCRenderState, t:Transformation, iconT:UVTransformation)
    {
        models("glass").render(ccrs, t, iconT)
    }
}
object RenderICPrinterItem extends IItemRenderer {

    //A single instance of this across reloads is fine.
    val wrapped = new CCBakeryModel()
    var entity:EntityLivingBase = _
    var world:World = _

    lazy val overrideList = new ItemOverrideList() {
        override def handleItemState(originalModel: IBakedModel, stack: ItemStack, world: World, entity: EntityLivingBase) = {
            RenderICPrinterItem.entity = entity
            RenderICPrinterItem.world = if(world == null) if(entity == null) null else entity.world else null
            originalModel
        }
    }

    override def renderItem(stack: ItemStack, transformType: ItemCameraTransforms.TransformType) {
        val model = wrapped.getOverrides.handleItemState(wrapped, stack, world, entity)
        Minecraft.getMinecraft.getRenderItem.renderModel(model, stack)
        val ccrs = CCRenderState.instance()
        val matrix = new Matrix4()
        val iconT = new MultiIconTransformation(RenderICPrinter.headIcon)

        ccrs.reset()
        ccrs.pullLightmap()
        ccrs.startDrawing(0x7, DefaultVertexFormats.ITEM)
        RenderICPrinterDynamic.renderPrinter(ccrs, matrix, iconT)
        ccrs.draw()
    }

    override def getTransforms = TransformUtils.DEFAULT_BLOCK

    override def isAmbientOcclusion = true

    override def isGui3d = true

    override def getOverrides = overrideList
}
