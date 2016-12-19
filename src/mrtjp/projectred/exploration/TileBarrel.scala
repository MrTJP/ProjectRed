/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.exploration

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.state.GlStateManagerHelper
import codechicken.lib.render.state.GlStateManagerHelper.State
import codechicken.lib.util.ItemUtils
import codechicken.lib.vec._
import mrtjp.core.block.{ItemBlockCore, MTBlockTile, MultiTileBlock}
import mrtjp.core.inventory.{IInvWrapperRegister, InvWrapper, TInventory}
import mrtjp.core.item.ItemKey
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExploration
import mrtjp.projectred.core.libmc.PRLib
import net.minecraft.block.SoundType
import net.minecraft.block.material.Material
import net.minecraft.block.state.IBlockState
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{IInventory, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.{BlockRenderLayer, EnumBlockRenderType, EnumFacing}
import org.lwjgl.opengl.GL11._

class BlockBarrel extends MultiTileBlock("projectred.exploration.barrel", "barrel", Material.WOOD)
{
    setHardness(2.0F)
    setSoundType(SoundType.WOOD)
    setCreativeTab(ProjectRedExploration.tabExploration)
    new ItemBlockCore(this)
    //TODO Remove on MultiTile rewrite
    override def getRenderType(state: IBlockState) = EnumBlockRenderType.MODEL

    override def canRenderInLayer(state: IBlockState, layer: BlockRenderLayer) = layer == BlockRenderLayer.SOLID
}

class TileBarrel extends MTBlockTile with TInventory with ISidedInventory
{
    var storage = 0
    var item:ItemKey = null

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setInteger("storage", storage)
        saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        storage = tag.getInteger("storage")
        loadInv(tag)
        refreshItemKey()
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeBoolean(nonEmpty)
        if (nonEmpty)
        {
            out.writeItemStack(item.makeStack(0))
            out.writeInt(getStoredAmount)
        }
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        silentClear()
        if (in.readBoolean())
        {
            item = ItemKey.get(in.readItemStack())
            storage = in.readInt()
            compactItems()
        }
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 1 => silentClear()
        case 2 =>
            silentClear()
            item = ItemKey.get(in.readItemStack())
            storage = in.readInt()
            compactItems()
        case _ => super.read(in, key)
    }

    override def getDisplayName = super.getDisplayName

    def sendItemUpdate()
    {
        if (isEmpty)
            writeStream(1).sendToChunk(this)
        else
            writeStream(2).writeItemStack(item.makeStack(0)).writeInt(getStoredAmount).sendToChunk(this)
    }

    override def getBlock = ProjectRedExploration.blockBarrel

    override def size = 2
    override def name = "barrel"

    override def canInsertItem(slot:Int, stack:ItemStack, side:EnumFacing) =
        slot == 0 && side == EnumFacing.UP && (isEmpty || ItemKey.get(stack) == item)
    override def canExtractItem(slot:Int, stack:ItemStack, side:EnumFacing) =
        slot == 1 && side == EnumFacing.DOWN && (isEmpty || ItemKey.get(stack) == item)

    override def getSlotsForFace(side:EnumFacing) =
        if (side == EnumFacing.DOWN) Array(1) else if (side == EnumFacing.UP) Array(0) else Array.empty

    def getStackSpace = 128

    def getFreeSpace =
        if (isEmpty) Int.MaxValue
        else getStackSpace*item.getMaxStackSize-getStoredAmount

    def getFreeStorageSpace =
        if (isEmpty) Int.MaxValue
        else (getStackSpace-2)*item.getMaxStackSize-storage

    def getStoredAmount =
    {
        var i = storage
        if (getStackInSlot(0) != null) i += getStackInSlot(0).stackSize
        if (getStackInSlot(1) != null) i += getStackInSlot(1).stackSize
        i
    }

    def getStoredFullStacks =
        if (isEmpty) 0
        else getStoredAmount/item.getMaxStackSize

    def getRemainderStacks =
        if (isEmpty) 0
        else getStoredAmount%item.getMaxStackSize

    def getDoubleClickTicks = 8

    def isEmpty = getStoredAmount == 0
    def nonEmpty = !isEmpty

    override def onBlockActivated(player:EntityPlayer, side:Int):Boolean =
    {
        if (super.onBlockActivated(player, side)) return true
        if (world.isRemote) return true
        //TODO Hand passthrough
        if ((checkDoubleClick() && importAll(player) > 0) ||
                importStack(ItemUtils.getHeldStack(player)) > 0) needsUpdate = true

        true
    }

    override def onBlockClicked(player:EntityPlayer):Boolean =
    {
        if (super.onBlockClicked(player)) return true
        if (world.isRemote) return true
        if (isEmpty) return false

        val inslot = getStackInSlot(1)
        val stored = getStoredAmount
        val toRem = math.min(
            if (player.isSneaking) 1
            else inslot.stackSize-(if(stored > 1 && stored <= inslot.getMaxStackSize) 1 else 0),
            inslot.getMaxStackSize
        )
        val out = inslot.splitStack(toRem)

        setInventorySlotContents(1, if (inslot.stackSize <= 0) null else inslot)

        PRLib.dropTowardsPlayer(world, getPos, out, player)
        needsUpdate = true
        true
    }


    override def onBlockRemoval()
    {
        super.onBlockRemoval()

        if (!world.isRemote)
        {
            var stack = getStackInSlot(1)
            while(stack != null && nonEmpty)
            {
                WorldLib.dropItem(world, getPos, stack)
                setInventorySlotContents(1, null)
                stack = getStackInSlot(1)
            }
        }
    }

    private var needsUpdate = false
    override def updateServer()
    {
        if (needsUpdate) sendItemUpdate()
        needsUpdate = false
    }

    private var lastActivateTime = -1L
    def checkDoubleClick():Boolean =
    {
        val dc = world.getTotalWorldTime-lastActivateTime <= getDoubleClickTicks
        lastActivateTime = world.getTotalWorldTime
        dc && item != null
    }

    def importAll(player:EntityPlayer) =
    {
        var s = 0
        for (i <- 0 until player.inventory.getSizeInventory)
        {
            val stack = player.inventory.getStackInSlot(i)
            if (stack != null)
            {
                s += importStack(stack)
                if (stack.stackSize <= 0)
                    player.inventory.setInventorySlotContents(i, null)
            }
        }
        if (s > 0) player.inventoryContainer.detectAndSendChanges()
        s
    }

    def importStack(stack:ItemStack):Int =
    {
        if (stack == null) return 0
        if (nonEmpty && !InvWrapper.areItemsSame(stack, item.makeStack(0))) return 0
        var inslot = getStackInSlot(0)
        if (inslot == null) inslot = stack.splitStack(0)
        val space = inslot.getMaxStackSize-inslot.stackSize
        val toAdd = math.min(space, stack.stackSize)
        if (toAdd > 0)
        {
            inslot.stackSize += toAdd
            stack.stackSize -= toAdd
            setInventorySlotContents(0, inslot)
        }
        toAdd
    }

    private var compacting = false
    override def markDirty()
    {
        super.markDirty()
        compactItems()
        needsUpdate = true
    }

    def refreshItemKey()
    {
        val inslot = getStackInSlot(1)
        item = if (inslot == null) null else ItemKey.get(inslot)
    }

    def compactItems()
    {
        if (compacting) return
        compacting = true

        if (getStackInSlot(0) != null)
        {
            val in = getStackInSlot(0)
            var out = getStackInSlot(1)

            if (out == null || !InvWrapper.areItemsSame(in, out)) out = in.splitStack(0)
            val toAdd = math.min(in.stackSize, out.getMaxStackSize-out.stackSize)
            if (toAdd > 0)
            {
                in.stackSize -= toAdd
                out.stackSize += toAdd
            }
            setInventorySlotContents(1, out)

            refreshItemKey()

            val sAdd = math.min(in.stackSize, getFreeStorageSpace)
            if (sAdd > 0)
            {
                in.stackSize -= sAdd
                storage += sAdd
            }

            if (in.stackSize == 0) setInventorySlotContents(0, null)
        }

        if (storage > 0)
        {
            var out = getStackInSlot(1)
            if (out == null) out = item.makeStack(0)
            val toAdd = math.min(storage, out.getMaxStackSize-out.stackSize)
            if (toAdd > 0)
            {
                out.stackSize += toAdd
                storage -= toAdd
                setInventorySlotContents(1, out)
            }
        }

        refreshItemKey()

        //cleanup
        for (i <- 0 until getSizeInventory)
            if (getStackInSlot(i) != null && getStackInSlot(i).stackSize <= 0)
                setInventorySlotContents(i, null)

        compacting = false
    }

    def silentClear()
    {
        item = null
        storage = 0
        compacting = true //dont run compactItems
        setInventorySlotContents(0, null)
        setInventorySlotContents(1, null)
        compacting = false
    }
}

object RenderBarrel extends TileEntitySpecialRenderer[TileBarrel] //with TCubeMapRender
{
    //var top:IIcon = null
    //var side:IIcon = null
    //var iconT:UVTransformation = null

    //override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) = (0, 0, iconT)

    //override def getInvData = (0, 0, iconT)

    //override def getIcon(s:Int, meta:Int) = if (s == 0 || s == 1) top else side

    //override def registerIcons(reg:IIconRegister)
    //{
    //    top = reg.registerIcon("projectred:world/barrel/top")
    //    side = reg.registerIcon("projectred:world/barrel/side")
    //    iconT = new MultiIconTransformation(top, top, side, side, side, side)
    //}

    //private val renderItem = new RenderItem
    //private val renderBlocks = new RenderBlocks

    override def renderTileEntityAt(tile:TileBarrel, x:Double, y:Double, z:Double, frame:Float, destroyProgress:Int)
    {
        if (tile.item == null) return

        val stack = tile.item.makeStack(1)
        val fr = Minecraft.getMinecraft.fontRendererObj
        val tm = Minecraft.getMinecraft.getTextureManager

        val stackSize = tile.item.getMaxStackSize
        val stacks = tile.getStoredFullStacks
        val extra = tile.getRemainderStacks
        val total = tile.getStoredAmount

        val text =
            if (total > 0)
            {
                if (stackSize == 1 || stacks == 0) s"$total"
                else if (extra == 0) s"$stacks x $stackSize"
                else s"$stacks x $stackSize + $extra"
            }
            else ""

        val tw = GuiDraw.getStringWidth(text)
        val tsc = 0.875/math.max(85, tw)

        val label = tile.item.getName
        val lw = GuiDraw.getStringWidth(label)
        val lsc = 0.875/math.max(128, lw)

        val faceT = new TransformationList(
            new Rotation(math.Pi, 1, 0, 0),
            new Translation(0, 1, 1)
        )

        val itemT = new TransformationList(
            new Scale(1/16D, 1/16D, -1.0E-04F) at new Vector3(0, 1, 1),
            new Scale(1/2D, 1/2D, 1) at Vector3.center,
            new Translation(0, 0.05, 0.0005)
        )

        val textT = new TransformationList(
            new Scale(tsc, tsc, 1) at new Vector3(0, 1, 1),
            new Translation(0.5-tw*tsc/2.0, -0.5/16D, 0.001)
        )

        val labelT = new TransformationList(
            new Scale(lsc, lsc, 1) at new Vector3(0, 1, 1),
            new Translation(0.5-lw*lsc/2.0, -14.5/16D, 0.001)
        )

        for (i <- 0 until 4)
        {
            val finalT = new TransformationList(
                Rotation.quarterRotations(i).at(Vector3.center),
                new Translation(x, y, z)
            )

            GlStateManagerHelper.pushStates(State.GL_ALPHA_TEST, State.GL_BLEND, State.GL_LIGHTING)
            glDisable(GL_BLEND)
            glDisable(GL_LIGHTING)
            glColor4d(1, 1, 1, 1)

            glPushMatrix()
            new TransformationList(faceT, itemT, finalT).glApply()
            Minecraft.getMinecraft.getRenderItem.renderItemAndEffectIntoGUI(stack, 0,0)
            glPopMatrix()

            glPushMatrix()
            new TransformationList(faceT, textT, finalT).glApply()
            GuiDraw.drawString(text, 0, 0, 0xFFFFFFFF, false)
            glPopMatrix()

            glPushMatrix()
            new TransformationList(faceT, labelT, finalT).glApply()
            GuiDraw.drawString(label, 0, 0, 0xFFFFFFFF, false)
            glPopMatrix()

            GlStateManagerHelper.popState()
        }
    }
}

object BarrelInvWrapper extends IInvWrapperRegister
{
    override def wrapperID = "pr_barrel"

    override def matches(inv:IInventory) = inv.isInstanceOf[TileBarrel]

    override def create(inv:IInventory) = new BarrelInvWrapper(inv)
}

class BarrelInvWrapper(inv:IInventory) extends InvWrapper(inv)
{
    def getBarrel = inv.asInstanceOf[TileBarrel]

    override def getSpaceForItem(item:ItemKey) =
        if (slots.contains(0) && (getBarrel.isEmpty || item == getBarrel.item)) getBarrel.getFreeSpace else 0

    override def hasSpaceForItem(item:ItemKey) = getSpaceForItem(item) > 0

    override def getItemCount(item:ItemKey) =
    {
        var count = if (slots.contains(1) && getBarrel.nonEmpty && eq.matches(item, getBarrel.item)) getBarrel.getStoredAmount else 0
        if (hidePerSlot || hidePerType) count -= 1
        math.max(count, 0)
    }

    override def hasItem(item:ItemKey) = getItemCount(item) > 0

    override def injectItem(item:ItemKey, toAdd:Int) =
    {
        var itemsLeft = toAdd
        if (slots.contains(0) && (getBarrel.isEmpty ||item == getBarrel.item))
        {
            import scala.util.control.Breaks._
            breakable { while (itemsLeft > 0 && getBarrel.getFreeSpace > 0)
            {
                val toAdd = math.min(item.getMaxStackSize, itemsLeft)
                val added = getBarrel.importStack(item.makeStack(toAdd))
                itemsLeft -= added
            }}
        }
        toAdd-itemsLeft
    }

    override def extractItem(item:ItemKey, toExtract:Int) =
    {
        var itemsLeft = toExtract
        val hidden = if (hidePerSlot || hidePerType) 1 else 0

        if (slots.contains(1) && eq.matches(item, getBarrel.item))
        {
            while(itemsLeft > 0 && getBarrel.getStoredAmount-hidden > 0)
            {
                var toRem = math.min(itemsLeft, getBarrel.getStoredAmount)
                toRem = math.min(toRem, item.getMaxStackSize)

                val bottomStack = getBarrel.getStackInSlot(1)
                bottomStack.stackSize -= toRem
                itemsLeft -= toRem

                if (bottomStack.stackSize <= 0) getBarrel.setInventorySlotContents(1, null)
                else getBarrel.markDirty()
            }
        }
        toExtract-itemsLeft
    }

    override def getAllItemStacks:Map[ItemKey, Int] =
    {
        if (slots.contains(1) && getBarrel.nonEmpty)
        {
            val count = getBarrel.getStoredAmount-(if (hidePerSlot || hidePerType) 1 else 0)
            if (count > 0) return Map(getBarrel.item -> count)
        }
        Map.empty
    }
}
