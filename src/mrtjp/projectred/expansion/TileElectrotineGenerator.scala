/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.model.bakery.SimpleBlockRenderer
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import mrtjp.core.gui._
import mrtjp.core.inventory.TInventory
import mrtjp.core.vec.Point
import mrtjp.core.world.WorldLib
import mrtjp.projectred.core.PartDefs
import mrtjp.projectred.{ProjectRedCore, ProjectRedExpansion}
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.ResourceLocation
import net.minecraft.util.math.BlockPos
import net.minecraft.world.IBlockAccess
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class TileElectrotineGenerator extends TPoweredMachine with TGuiMachine with TInventory
{
    var isBurning = false
    var isCharged = false
    var burnTimeRemaining = 0
    var powerStorage = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        saveInv(tag)
        tag.setInteger("storage", powerStorage)
        tag.setShort("btime", burnTimeRemaining.toShort)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        loadInv(tag)
        powerStorage = tag.getInteger("storage")
        burnTimeRemaining = tag.getShort("btime")
        isBurning = burnTimeRemaining > 0
        isCharged = cond.canWork
        ib = isBurning
        ic = isCharged
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeBoolean(isCharged)
        out.writeBoolean(isBurning)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        isCharged = in.readBoolean()
        isBurning = in.readBoolean()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 5 =>
            isCharged = in.readBoolean()
            isBurning = in.readBoolean()
            markRender()
            markLight()
        case _ => super.read(in, key)
    }

    def sendRenderUpdate()
    {
        writeStream(5).writeBoolean(isCharged).writeBoolean(isBurning).sendToChunk(this)
    }

    override def getBlock = ProjectRedExpansion.machine1

    override def openGui(player:EntityPlayer) =
        GuiElectrotineGenerator.open(player, createContainer(player), _.writePos(getPos))

    override def createContainer(player:EntityPlayer) =
        new ContainerElectrotineGenerator(player, this)

    override protected val storage = Array.fill(1)(ItemStack.EMPTY)//new Array[ItemStack](1)
    override def getInventoryStackLimit = 64
    override def getName = "electrotine_generator"

    override def getDisplayName = super.getDisplayName
    override def isItemValidForSlot(slot:Int, stack:ItemStack) =
        stack != null && stack.getItem == ProjectRedCore.itemPart &&
            stack.getItemDamage == PartDefs.ELECTROTINE.meta

    def getStorageScaled(i:Int) = math.min(i, i*powerStorage/getMaxStorage)
    def getBurnTimeScaled(i:Int) = math.min(i, i*burnTimeRemaining/getBurnTimePerDust)

    def getMaxStorage = 800
    def getBurnTimePerDust = 2750
    def getBurnUseOnCharge = 10
    def getBurnUseOnIdle = 1
    def getDrawSpeed = 100
    def getDrawFloor = 1000

    override def updateServer()
    {
        super.updateServer()

        tryBurnDust()
        tryChargeStorage()
        tryChargeConductor()
        tryBurnDust()

        if (world.getTotalWorldTime%10 == 0) updateRenderIfNeeded()
    }

    def tryBurnDust()
    {
        if (powerStorage < getMaxStorage && burnTimeRemaining < getBurnUseOnCharge)
        {
            val inslot = getStackInSlot(0)
            if (!inslot.isEmpty)
            {
                inslot.shrink(1)
                burnTimeRemaining = getBurnTimePerDust
                if (inslot.isEmpty) setInventorySlotContents(0, ItemStack.EMPTY)
                else setInventorySlotContents(0, inslot)
            }
        }
    }

    def tryChargeStorage()
    {
        if (burnTimeRemaining > 0)
        {
            if (powerStorage < getMaxStorage && burnTimeRemaining >= getBurnUseOnCharge)
            {
                powerStorage += 1
                burnTimeRemaining -= getBurnUseOnCharge
            }
            else
            {
                burnTimeRemaining -= getBurnUseOnIdle
            }
        }
    }

    def tryChargeConductor()
    {
        if (cond.charge < getDrawFloor && powerStorage > 0)
        {
            var n = math.min(getDrawFloor-cond.charge, getDrawSpeed)/10
            n = math.min(n, powerStorage)
            cond.applyPower(n*1000)
            powerStorage -= n
        }
    }

    private var ib = false
    private var ic = false
    def updateRenderIfNeeded()
    {
        isCharged = cond.canWork
        isBurning = burnTimeRemaining > 0
        if (ib != isBurning || ic != isCharged) sendRenderUpdate()
        ib = isBurning
        ic = isCharged
    }

    override def getLightValue = if (isBurning) 13 else 0

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(world, getPos)
    }
}

class ContainerElectrotineGenerator(p:EntityPlayer, tile:TileElectrotineGenerator) extends ContainerPoweredMachine(tile)
{
    {
        addSlotToContainer(new Slot3(tile, 0, 134, 42))
        addPlayerInv(p, 8, 89)
    }

    private var st = -1
    private var bt = -1
    override def detectAndSendChanges()
    {
        super.detectAndSendChanges()
        import scala.collection.JavaConversions._
        for (i <- listeners)
        {
            if (st != tile.powerStorage) i
                    .sendProgressBarUpdate(this, 3, tile.powerStorage)
            if (bt != tile.burnTimeRemaining) i
                    .sendProgressBarUpdate(this, 4, tile.burnTimeRemaining)
        }
        st = tile.powerStorage
        bt = tile.burnTimeRemaining
    }

    override def updateProgressBar(id:Int, bar:Int) = id match
    {
        case 3 => tile.powerStorage = bar
        case 4 => tile.burnTimeRemaining = bar
        case _ => super.updateProgressBar(id, bar)
    }
}

class GuiElectrotineGenerator(tile:TileElectrotineGenerator, c:ContainerElectrotineGenerator) extends NodeGui(c, 176, 171)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        TextureUtils.changeTexture(GuiElectrotineGenerator.background)
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)

        if (tile.cond.canWork)
            GuiDraw.drawTexturedModalRect(22, 16, 176, 1, 7, 9)
        GuiLib.drawVerticalTank(22, 26, 176, 10, 7, 48, tile.cond.getChargeScaled(48))

        if (tile.powerStorage == tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(54, 16, 184, 1, 14, 9)
        GuiLib.drawVerticalTank(54, 26, 184, 10, 14, 48, tile.getStorageScaled(48))

        if (tile.burnTimeRemaining > 0)
            GuiDraw.drawTexturedModalRect(93, 16, 199, 1, 7, 9)
        GuiLib.drawVerticalTank(93, 26, 199, 10, 7, 48, tile.getBurnTimeScaled(48))

        if (tile.cond.charge < tile.getDrawFloor && (tile.powerStorage > 0 || tile.burnTimeRemaining > tile.getBurnUseOnCharge))
            GuiDraw.drawTexturedModalRect(30, 46, 211, 0, 23, 9)

        if (tile.burnTimeRemaining > tile.getBurnUseOnCharge && tile.powerStorage < tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(69, 45, 211, 10, 23, 9)

        GuiDraw.drawString("Electrotine Generator", 8, 6, EnumColour.GRAY.argb, false)
        GuiDraw.drawString("Inventory", 8, 79, EnumColour.GRAY.argb, false)
    }
}

object GuiElectrotineGenerator extends TGuiFactory
{
    val background = new ResourceLocation("projectred", "textures/gui/electrotine_generator.png")
    override def getID = ExpansionProxy.generatorGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        player.world.getTileEntity(data.readPos()) match
        {
            case t:TileElectrotineGenerator => new GuiElectrotineGenerator(t, t.createContainer(player))
            case _ => null
        }
    }
}

object RenderElectrotineGenerator extends SimpleBlockRenderer
{
    import org.apache.commons.lang3.tuple.Triple
    import mrtjp.projectred.expansion.BlockProperties._
    import java.lang.{Boolean => JBool, Integer => JInt}

    var bottom:TextureAtlasSprite = _
    var top:TextureAtlasSprite = _
    var side1:TextureAtlasSprite = _
    var side2a:TextureAtlasSprite = _
    var side2b:TextureAtlasSprite = _
    var side2c:TextureAtlasSprite = _
    var side2d:TextureAtlasSprite = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _
    var iconT3:UVTransformation = _
    var iconT4:UVTransformation = _


    override def handleState(state: IExtendedBlockState, world:IBlockAccess, pos:BlockPos): IExtendedBlockState = world.getTileEntity(pos) match {
        case t:TileElectrotineGenerator => {
            var s = state
            s = s.withProperty(UNLISTED_CHARGED_PROPERTY, t.isCharged.asInstanceOf[JBool])
            s = s.withProperty(UNLISTED_BURNING_PROPERTY, t.isBurning.asInstanceOf[JBool])
            s = s.withProperty(UNLISTED_SIDE_PROPERTY, t.side.asInstanceOf[JInt])
            s.withProperty(UNLISTED_ROTATION_PROPERTY, t.rotation.asInstanceOf[JInt])
        }
        case _ => state
    }

    override def getWorldTransforms(state: IExtendedBlockState) = {
        val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY).asInstanceOf[Boolean]
        val isBurning = state.getValue(UNLISTED_BURNING_PROPERTY).asInstanceOf[Boolean]
        val side = state.getValue(UNLISTED_SIDE_PROPERTY)
        val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)

        val iconT = (isCharged, isBurning) match
        {
            case (false, false) => iconT1
            case (true, false)  => iconT2
            case (false, true)  => iconT3
            case (true, true)   => iconT4
        }
        Triple.of(side, rotation, iconT)
    }

    override def getItemTransforms(stack: ItemStack) = Triple.of(0, 0, iconT1)

    override def shouldCull() = true

    override def registerIcons(reg:TextureMap)
    {
        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/bottom"))
        top = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/top"))
        side1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/side1"))
        side2a = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/side2a"))
        side2b = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/side2b"))
        side2c = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/side2c"))
        side2d = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/side2d"))

        iconT1 = new MultiIconTransformation(bottom, top, side1, side2a, side1, side1)
        iconT2 = new MultiIconTransformation(bottom, top, side1, side2b, side1, side1)
        iconT3 = new MultiIconTransformation(bottom, top, side1, side2c, side1, side1)
        iconT4 = new MultiIconTransformation(bottom, top, side1, side2d, side1, side1)
    }
}
