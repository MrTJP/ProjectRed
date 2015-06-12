/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.uv.{MultiIconTransformation, UVTransformation}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.gui.{GuiLib, Slot3, TGuiBuilder, WidgetGui}
import mrtjp.core.inventory.TInventory
import mrtjp.core.render.TCubeMapRender
import mrtjp.core.vec.Point
import mrtjp.core.world.WorldLib
import mrtjp.projectred.core.PartDefs
import mrtjp.projectred.core.libmc.PRResources
import mrtjp.projectred.{ProjectRedCore, ProjectRedExpansion}
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.ICrafting
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.IIcon
import net.minecraft.world.IBlockAccess

class TileElectrotineGenerator extends TPoweredMachine with TGuiMachine with TInventory
{
    var isBurning = false
    var isCharged = false
    var burnTimeRemaining = 0
    var storage = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        saveInv(tag)
        tag.setInteger("storage", storage)
        tag.setShort("btime", burnTimeRemaining.toShort)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        loadInv(tag)
        storage = tag.getInteger("storage")
        burnTimeRemaining = tag.getShort("btime")
        isBurning = burnTimeRemaining > 0
        isCharged = cond.canWork
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
        writeStream(5).writeBoolean(isCharged).writeBoolean(isBurning).sendToChunk()
    }

    override def getBlock = ProjectRedExpansion.machine1

    override def openGui(player:EntityPlayer) =
        GuiElectrotineGenerator.open(player, createContainer(player), _.writeCoord(x, y, z))

    override def createContainer(player:EntityPlayer) =
        new ContainerElectrotineGenerator(player, this)

    override def size = 1
    override def name = "electrotine_generator"

    override def isItemValidForSlot(slot:Int, stack:ItemStack) =
        stack != null && stack.getItem == ProjectRedCore.itemPart &&
            stack.getItemDamage == PartDefs.ELECTROTINE.meta

    def getStorageScaled(i:Int) = math.min(i, i*storage/getMaxStorage)
    def getBurnTimeScaled(i:Int) = math.min(i, i*burnTimeRemaining/getBurnTimePerDust)

    def getMaxStorage = 800
    def getBurnTimePerDust = 350
    def getBurnUseOnCharge = 10
    def getBurnUseOnIdle = 1
    def getDrawSpeed = 100
    def getDrawFloor = 1000

    override def update()
    {
        super.update()

        tryBurnDust()
        tryChargeStorage()
        tryChargeConductor()
        tryBurnDust()

        if (world.getTotalWorldTime%10 == 0) updateRenderIfNeeded()
    }

    def tryBurnDust()
    {
        if (storage < getMaxStorage && burnTimeRemaining < getBurnUseOnCharge)
        {
            val inslot = getStackInSlot(0)
            if (inslot != null)
            {
                inslot.stackSize -= 1
                burnTimeRemaining = getBurnTimePerDust
                if (inslot.stackSize == 0) setInventorySlotContents(0, null)
                else setInventorySlotContents(0, inslot)
            }
        }
    }

    def tryChargeStorage()
    {
        if (burnTimeRemaining > 0)
        {
            if (storage < getMaxStorage && burnTimeRemaining >= getBurnUseOnCharge)
            {
                storage += 1
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
        if (cond.charge < getDrawFloor && storage > 0)
        {
            var n = math.min(getDrawFloor-cond.charge, getDrawSpeed)/10
            n = math.min(n, storage)
            cond.applyPower(n*1000)
            storage -= n
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
}

class ContainerElectrotineGenerator(p:EntityPlayer, tile:TileElectrotineGenerator) extends ContainerPoweredMachine(p, tile)
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
        for (i <- crafters)
        {
            if (st != tile.storage) i.asInstanceOf[ICrafting]
                    .sendProgressBarUpdate(this, 3, tile.storage)
            if (bt != tile.burnTimeRemaining) i.asInstanceOf[ICrafting]
                    .sendProgressBarUpdate(this, 4, tile.burnTimeRemaining)
        }
        st = tile.storage
        bt = tile.burnTimeRemaining
    }

    override def updateProgressBar(id:Int, bar:Int) = id match
    {
        case 3 => tile.storage = bar
        case 4 => tile.burnTimeRemaining = bar
        case _ => super.updateProgressBar(id, bar)
    }
}

class GuiElectrotineGenerator(tile:TileElectrotineGenerator, c:ContainerElectrotineGenerator) extends WidgetGui(c, 176, 171)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        PRResources.guiElectrotineGenerator.bind()
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)

        if (tile.cond.canWork)
            GuiDraw.drawTexturedModalRect(22, 16, 176, 1, 7, 9)
        GuiLib.drawVerticalTank(22, 26, 176, 10, 7, 48, tile.cond.getChargeScaled(48))

        if (tile.storage == tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(54, 16, 184, 1, 14, 9)
        GuiLib.drawVerticalTank(54, 26, 184, 10, 14, 48, tile.getStorageScaled(48))

        if (tile.burnTimeRemaining > 0)
            GuiDraw.drawTexturedModalRect(93, 16, 199, 1, 7, 9)
        GuiLib.drawVerticalTank(93, 26, 199, 10, 7, 48, tile.getBurnTimeScaled(48))

        if (tile.cond.charge < tile.getDrawFloor && (tile.storage > 0 || tile.burnTimeRemaining > tile.getBurnUseOnCharge))
            GuiDraw.drawTexturedModalRect(30, 46, 211, 0, 23, 9)

        if (tile.burnTimeRemaining > tile.getBurnUseOnCharge && tile.storage < tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(69, 45, 211, 10, 23, 9)

        GuiDraw.drawString("Electrotine Generator", 8, 6, Colors.GREY.argb, false)
        GuiDraw.drawString("Inventory", 8, 79, Colors.GREY.argb, false)
    }
}

object GuiElectrotineGenerator extends TGuiBuilder
{
    override def getID = ExpansionProxy.generatorGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        WorldLib.getTileEntity(player.worldObj, data.readCoord()) match
        {
            case t:TileElectrotineGenerator => new GuiElectrotineGenerator(t, t.createContainer(player))
            case _ => null
        }
    }
}

object RenderElectrotineGenerator extends TCubeMapRender
{
    var bottom:IIcon = _
    var top:IIcon = _
    var side1:IIcon = _
    var side2a:IIcon = _
    var side2b:IIcon = _
    var side2c:IIcon = _
    var side2d:IIcon = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _
    var iconT3:UVTransformation = _
    var iconT4:UVTransformation = _

    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TileElectrotineGenerator])

        if (te != null)
        {
            val iconT = (te.isCharged, te.isBurning) match
            {
                case (false, false) => iconT1
                case (true, false)  => iconT2
                case (false, true)  => iconT3
                case (true, true)   => iconT4
            }
            (te.side, te.rotation, iconT)
        }
        else (0, 0, iconT1)
    }

    override def getInvData = (0, 0, iconT1)

    override def getIcon(side:Int, meta:Int) = side match
    {
        case 0 => bottom
        case 1 => top
        case _ => side1
    }

    override def registerIcons(reg:IIconRegister)
    {
        bottom = reg.registerIcon("projectred:machines/elecgen/bottom")
        top = reg.registerIcon("projectred:machines/elecgen/top")
        side1 = reg.registerIcon("projectred:machines/elecgen/side1")
        side2a = reg.registerIcon("projectred:machines/elecgen/side2a")
        side2b = reg.registerIcon("projectred:machines/elecgen/side2b")
        side2c = reg.registerIcon("projectred:machines/elecgen/side2c")
        side2d = reg.registerIcon("projectred:machines/elecgen/side2d")

        iconT1 = new MultiIconTransformation(bottom, top, side1, side2a, side1, side1)
        iconT2 = new MultiIconTransformation(bottom, top, side1, side2b, side1, side1)
        iconT3 = new MultiIconTransformation(bottom, top, side1, side2c, side1, side1)
        iconT4 = new MultiIconTransformation(bottom, top, side1, side2d, side1, side1)
    }
}