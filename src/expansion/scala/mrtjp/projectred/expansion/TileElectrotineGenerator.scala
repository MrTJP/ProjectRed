/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.inventory.container.ICCLContainerFactory
import codechicken.lib.texture.TextureUtils
import mrtjp.core.gui._
import mrtjp.core.inventory.{TInventory, TInventoryCapablilityTile}
import mrtjp.core.vec.Point
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.CoreContent
import net.minecraft.client.gui.ScreenManager
import net.minecraft.entity.player.{PlayerEntity, PlayerInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.ResourceLocation
import net.minecraft.util.text.ITextComponent

import scala.jdk.CollectionConverters._

class TileElectrotineGenerator extends TileMachine(ExpansionContent.electrotineGeneratorTile.get) with TPoweredMachine with TGuiMachine with TInventory with TInventoryCapablilityTile
{
    var isBurning = false
    var isCharged = false
    var burnTimeRemaining = 0
    var powerStorage = 0

    override protected val storage:Array[ItemStack] = Array.fill(1)(ItemStack.EMPTY)

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        saveInv(tag)
        tag.putInt("storage", powerStorage)
        tag.putShort("btime", burnTimeRemaining.toShort)
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        loadInv(tag)
        powerStorage = tag.getInt("storage")
        burnTimeRemaining = tag.getShort("btime")
        isBurning = burnTimeRemaining > 0
        isCharged = cond.canWork
        ib = isBurning
        ic = isCharged
    }

    override def writeDesc(out:MCDataOutput):Unit = {
        super.writeDesc(out)
        out.writeBoolean(isCharged)
        out.writeBoolean(isBurning)
    }

    override def readDesc(in:MCDataInput):Unit = {
        super.readDesc(in)
        isCharged = in.readBoolean()
        isBurning = in.readBoolean()
    }

    override def readUpdate(key:Int, in:MCDataInput):Unit = key match  {
        case 5 =>
            isCharged = in.readBoolean()
            isBurning = in.readBoolean()
//            markRender()
//            recalcLight(false, true)
        case _ => super.readUpdate(key, in)
    }

    def sendRenderUpdate():Unit = {
        sendUpdate(5, _.writeBoolean(isCharged).writeBoolean(isBurning))
    }

    override def createMenu(windowId:Int, playerInv:PlayerInventory, player:PlayerEntity):ContainerElectrotineGenerator =
        new ContainerElectrotineGenerator(playerInv, this, windowId)

    override def getInventoryStackLimit = 64
    override def nbtSaveName = "electrotine_generator"

    override def isItemValidForSlot(slot:Int, stack:ItemStack):Boolean =
        !stack.isEmpty && stack.getItem == CoreContent.itemElectrotineDust.get

    def getStorageScaled(i:Int):Int = math.min(i, i*powerStorage/getMaxStorage)
    def getBurnTimeScaled(i:Int):Int = math.min(i, i*burnTimeRemaining/getBurnTimePerDust)

    def getMaxStorage = 800
    def getBurnTimePerDust = 2750
    def getBurnUseOnCharge = 10
    def getBurnUseOnIdle = 1
    def getDrawSpeed = 100
    def getDrawFloor = 1000

    override def updateServer():Unit = {
        super.updateServer()

        tryBurnDust()
        tryChargeStorage()
        tryChargeConductor()
        tryBurnDust()

        if (world.getGameTime%10 == 0) updateRenderIfNeeded()
    }

    def tryBurnDust():Unit = {
        if (powerStorage < getMaxStorage && burnTimeRemaining < getBurnUseOnCharge)  {
            val inslot = getStackInSlot(0)
            if (!inslot.isEmpty) {
                inslot.shrink(1)
                burnTimeRemaining = getBurnTimePerDust
                if (inslot.isEmpty) setInventorySlotContents(0, ItemStack.EMPTY)
                else setInventorySlotContents(0, inslot)
            }
        }
    }

    def tryChargeStorage():Unit = {
        if (burnTimeRemaining > 0) {
            if (powerStorage < getMaxStorage && burnTimeRemaining >= getBurnUseOnCharge) {
                powerStorage += 1
                burnTimeRemaining -= getBurnUseOnCharge
            } else {
                burnTimeRemaining -= getBurnUseOnIdle
            }
        }
    }

    def tryChargeConductor():Unit = {
        if (cond.charge < getDrawFloor && powerStorage > 0) {
            var n = math.min(getDrawFloor-cond.charge, getDrawSpeed)/10
            n = math.min(n, powerStorage)
            cond.applyPower(n*1000)
            powerStorage -= n
        }
    }

    private var ib = false
    private var ic = false
    def updateRenderIfNeeded():Unit = {
        isCharged = cond.canWork
        isBurning = burnTimeRemaining > 0
        if (ib != isBurning || ic != isCharged) {
            sendRenderUpdate()
            pushState()
        }
        ib = isBurning
        ic = isCharged
    }

    override def getLightValue:Int = if (isBurning) 13 else 0

    override def onBlockRemoved():Unit = {
        super.onBlockRemoved()
        dropInvContents(world, getPos)
    }
}

class ContainerElectrotineGenerator(playerInv:PlayerInventory, val tile:TileElectrotineGenerator, windowId:Int) extends ContainerPoweredMachine(tile, ExpansionContent.electrotineGeneratorContainer.get, windowId)
{
    {
        addSlot(new Slot3(tile, 0, 134, 42))
        addPlayerInv(playerInv, 8, 89)
    }

    private var st = -1
    private var bt = -1
    override def detectAndSendChanges():Unit = {
        super.detectAndSendChanges()
        for (i <- listeners.asScala) {
            if (st != tile.powerStorage)
                i.sendWindowProperty(this, 3, tile.powerStorage)
            if (bt != tile.burnTimeRemaining)
                i.sendWindowProperty(this, 4, tile.burnTimeRemaining)
        }
        st = tile.powerStorage
        bt = tile.burnTimeRemaining
    }

    override def updateProgressBar(id:Int, bar:Int):Unit = id match {
        case 3 => tile.powerStorage = bar
        case 4 => tile.burnTimeRemaining = bar
        case _ => super.updateProgressBar(id, bar)
    }
}

object ContainerElectrotineGenerator extends ICCLContainerFactory[ContainerElectrotineGenerator]
{
    override def create(windowId:Int, inventory:PlayerInventory, packet:MCDataInput):ContainerElectrotineGenerator = {
        inventory.player.world.getTileEntity(packet.readPos()) match {
            case t:TileElectrotineGenerator => t.createMenu(windowId, inventory, inventory.player)
            case _ => null
        }
    }
}

class GuiElectrotineGenerator(c:ContainerElectrotineGenerator, playerInv:PlayerInventory, title:ITextComponent) extends NodeGui(c, 176, 171, playerInv, title)
{
    override def drawBack_Impl(mouse:Point, frame:Float):Unit = {
        TextureUtils.changeTexture(GuiElectrotineGenerator.background)
        blit(0, 0, 0, 0, size.width, size.height)

        if (c.tile.cond.canWork)
            blit(22, 16, 176, 1, 7, 9)
        GuiLib.drawVerticalTank(this, 22, 26, 176, 10, 7, 48, c.tile.cond.getChargeScaled(48))

        if (c.tile.powerStorage == c.tile.getMaxStorage)
            blit(54, 16, 184, 1, 14, 9)
        GuiLib.drawVerticalTank(this, 54, 26, 184, 10, 14, 48, c.tile.getStorageScaled(48))

        if (c.tile.burnTimeRemaining > 0)
            blit(93, 16, 199, 1, 7, 9)
        GuiLib.drawVerticalTank(this, 93, 26, 199, 10, 7, 48, c.tile.getBurnTimeScaled(48))

        if (c.tile.cond.charge < c.tile.getDrawFloor && (c.tile.powerStorage > 0 || c.tile.burnTimeRemaining > c.tile.getBurnUseOnCharge))
            blit(30, 46, 211, 0, 23, 9)

        if (c.tile.burnTimeRemaining > c.tile.getBurnUseOnCharge && c.tile.powerStorage < c.tile.getMaxStorage)
            blit(69, 45, 211, 10, 23, 9)

        getFontRenderer.drawString(title.getFormattedText, 8, 6, EnumColour.GRAY.argb)
        getFontRenderer.drawString(playerInv.getDisplayName.getFormattedText, 8, 79, EnumColour.GRAY.argb)
    }
}

object GuiElectrotineGenerator
{
    val background = new ResourceLocation(ProjectRedExpansion.MOD_ID, "textures/gui/electrotine_generator.png")

    def register():Unit = {
        ScreenManager.registerFactory(
            ExpansionContent.electrotineGeneratorContainer.get,
            (cont:ContainerElectrotineGenerator, inv, text) => new GuiElectrotineGenerator(cont, inv, text)
        )
    }
//    override def getID = ExpansionProxy.generatorGui
//
//    @SideOnly(Side.CLIENT)
//    override def buildGui(player:EntityPlayer, data:MCDataInput) =
//    {
//        player.world.getTileEntity(data.readPos()) match
//        {
//            case t:TileElectrotineGenerator => new GuiElectrotineGenerator(t, t.createContainer(player))
//            case _ => null
//        }
//    }
}

//object RenderElectrotineGenerator extends SimpleBlockRenderer
//{
//    import org.apache.commons.lang3.tuple.Triple
//
//    import java.lang.{Boolean => JBool, Integer => JInt}
//
//    var bottom:TextureAtlasSprite = _
//    var top:TextureAtlasSprite = _
//    var side1:TextureAtlasSprite = _
//    var side2a:TextureAtlasSprite = _
//    var side2b:TextureAtlasSprite = _
//    var side2c:TextureAtlasSprite = _
//    var side2d:TextureAtlasSprite = _
//
//    var iconT1:UVTransformation = _
//    var iconT2:UVTransformation = _
//    var iconT3:UVTransformation = _
//    var iconT4:UVTransformation = _
//
//
//    override def handleState(state: IExtendedBlockState, world:IBlockAccess, pos:BlockPos): IExtendedBlockState = world.getTileEntity(pos) match {
//        case t:TileElectrotineGenerator => {
//            var s = state
//            s = s.withProperty(UNLISTED_CHARGED_PROPERTY, t.isCharged.asInstanceOf[JBool])
//            s = s.withProperty(UNLISTED_BURNING_PROPERTY, t.isBurning.asInstanceOf[JBool])
//            s = s.withProperty(UNLISTED_SIDE_PROPERTY, t.side.asInstanceOf[JInt])
//            s.withProperty(UNLISTED_ROTATION_PROPERTY, t.rotation.asInstanceOf[JInt])
//        }
//        case _ => state
//    }
//
//    override def getWorldTransforms(state: IExtendedBlockState) = {
//        val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY).asInstanceOf[Boolean]
//        val isBurning = state.getValue(UNLISTED_BURNING_PROPERTY).asInstanceOf[Boolean]
//        val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//        val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//
//        val iconT = (isCharged, isBurning) match
//        {
//            case (false, false) => iconT1
//            case (true, false)  => iconT2
//            case (false, true)  => iconT3
//            case (true, true)   => iconT4
//        }
//        Triple.of(side, rotation, iconT)
//    }
//
//    override def getItemTransforms(stack: ItemStack) = Triple.of(0, 0, iconT1)
//
//    override def shouldCull() = true
//
//    override def registerIcons(reg:TextureMap)
//    {
//        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/bottom"))
//        top = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/top"))
//        side1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/side1"))
//        side2a = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/side2a"))
//        side2b = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/side2b"))
//        side2c = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/side2c"))
//        side2d = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/elecgen/side2d"))
//
//        iconT1 = new MultiIconTransformation(bottom, top, side1, side2a, side1, side1)
//        iconT2 = new MultiIconTransformation(bottom, top, side1, side2b, side1, side1)
//        iconT3 = new MultiIconTransformation(bottom, top, side1, side2c, side1, side1)
//        iconT4 = new MultiIconTransformation(bottom, top, side1, side2d, side1, side1)
//    }
//}
