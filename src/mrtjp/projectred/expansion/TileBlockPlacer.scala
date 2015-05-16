/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import java.util.UUID

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.raytracer.RayTracer
import codechicken.lib.render.uv.{MultiIconTransformation, UVTransformation}
import codechicken.lib.vec.{BlockCoord, Cuboid6, Vector3}
import com.mojang.authlib.GameProfile
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.block.TInstancedBlockRender
import mrtjp.core.color.Colors
import mrtjp.core.gui._
import mrtjp.core.inventory.{SimpleInventory, TPortableInventory}
import mrtjp.core.render.TCubeMapRender
import mrtjp.core.vec.Point
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.libmc.PRResources
import mrtjp.projectred.expansion.TileBlockPlacer._
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.{Entity, EntityLivingBase}
import net.minecraft.inventory.Container
import net.minecraft.item.{ItemBlock, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.IIcon
import net.minecraft.world.{IBlockAccess, WorldServer}
import net.minecraftforge.common.util.FakePlayerFactory
import net.minecraftforge.event.ForgeEventFactory
import net.minecraftforge.event.entity.player.PlayerInteractEvent

import scala.ref.WeakReference

class TileBlockPlacer extends TileMachine with TActiveDevice with TPortableInventory
{
    override def getBlock = ProjectRedExpansion.machine2
    override def doesRotate = false
    override def doesOrient = true

    override def createInv = new SimpleInventory(9)

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        inv.saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        inv.loadInv(tag)
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
    }

    override def markDirty()
    {
        super.markDirty()
    }

    def reloadPlayer()
    {
        if (fakePlayer == null)
            fake = WeakReference(FakePlayerFactory.get(world.asInstanceOf[WorldServer],
                new GameProfile(UUID.randomUUID(), "[PR_FAKE]")))

        val pos = new Vector3(x, y, z).add(positions(side))
        val (pitch, yaw) = angles(side)
        fakePlayer.setLocationAndAngles(pos.x, pos.y, pos.z, pitch, yaw)
    }

    override def onBlockActivated(player:EntityPlayer, actside:Int):Boolean =
    {
        if (super.onBlockActivated(player, actside)) return true

        if (!world.isRemote)
            GuiBlockPlacer.open(player, createContainer(player), _.writeCoord(x, y, z))
        true
    }

    def createContainer(player:EntityPlayer):Container =
    {
        val cont = new WidgetContainer
        var s = 0
        for ((x, y) <- GuiLib.createSlotGrid(62, 18, 3, 3, 0, 0))
        {
            cont + new Slot2(inv, s, x, y)
            s += 1
        }
        cont.addPlayerInv(player, 8, 86)
    }

    override def onActivate()
    {
        if (world.isRemote) return
        reloadPlayer()
        val upos = position.offset(side^1)
        copyInvToPlayer()
        for (i <- 0 until 9)
        {
            val stack = getStackInSlot(i)
            if (stack != null && tryUseItem(stack, upos.x, upos.y, upos.z, i))
            {
                if (fakePlayer.isUsingItem) fakePlayer.stopUsingItem()
                copyInvFromPlayer()
                val newStack = getStackInSlot(i)
                if (newStack != null && newStack.stackSize == 0)
                    setInventorySlotContents(i, null)
                return
            }
        }
        copyInvFromPlayer()
    }

    def copyInvToPlayer()
    {
        for (i <- 0 until 9)
            fakePlayer.inventory.setInventorySlotContents(i, getStackInSlot(i))
    }

    def copyInvFromPlayer()
    {
        for (i <- 0 until 9)
            setInventorySlotContents(i, fakePlayer.inventory.getStackInSlot(i))
    }

    def tryUseItem(stack:ItemStack, x:Int, y:Int, z:Int, slot:Int) =
    {
        fakePlayer.inventory.currentItem = slot

        tryRightClick(stack, x, y, z, slot) ||
            tryEntityClick(stack, x, y, z)
    }

    def tryRightClick(stack:ItemStack, x:Int, y:Int, z:Int, slot:Int):Boolean =
    {
        val event = ForgeEventFactory.onPlayerInteract(fakePlayer,
            PlayerInteractEvent.Action.RIGHT_CLICK_BLOCK, x, y, z, 1, world)
        if (event.isCanceled) return false

        if (stack.getItem.onItemUseFirst(stack, fakePlayer, world, x, y, z, 1, 0.5F, 0.5f, 0.5F)) return true

        if (stack.getItem.isInstanceOf[ItemBlock])
        {
            if (stack.tryPlaceItemIntoWorld(fakePlayer, world, x, y, z, 1, 0.5F, 0.5F, 0.5F)) return true
        }
        else
        {
            if (stack.getItem.onItemUse(stack, fakePlayer, world, x, y, z, 1, 0.5F, 0.5F, 0.5F)) return true
            if (stack.getItem.onItemUse(stack, fakePlayer, world, x, y-1, z, 1, 0.5F, 0.5F, 0.5F)) return true
        }

        val size = stack.stackSize
        val newStack = stack.useItemRightClick(world, fakePlayer)
        if (stack != newStack || (newStack != null && newStack.stackSize != size))
        {
            fakePlayer.inventory.setInventorySlotContents(slot, newStack)
            return true
        }

        false
    }

    def tryEntityClick(stack:ItemStack, x:Int, y:Int, z:Int):Boolean =
    {
        val start = position.toVector3Centered
        val end = start.copy.add(new Vector3(BlockCoord.sideOffsets(side^1)).multiply(2.5))
        val e = traceEntityHits(start, end)
        e != null && useOnEntity(stack, e)
    }

    def useOnEntity(stack:ItemStack, e:Entity):Boolean =
    {
        if (e.interactFirst(fakePlayer)) return true

        if (e.isInstanceOf[EntityLivingBase])
        {
            val oldS = stack.stackSize
            stack.interactWithEntity(fakePlayer, e.asInstanceOf[EntityLivingBase])
            if(stack.stackSize != oldS) return true
        }
        false
    }

    def traceEntityHits(start:Vector3, end:Vector3) =
    {
        val box = new Cuboid6(0, 1, 0, 1, 3.5, 1).apply(rotationT).add(new Vector3(position))
        val elist = world.getEntitiesWithinAABBExcludingEntity(fakePlayer, box.toAABB)

        var eHit:Entity = null
        var edis = 0.0D

        for (i <- 0 until elist.size())
        {
            val e = elist.get(i).asInstanceOf[Entity]
            if (e.canBeCollidedWith)
            {
                val a2 = new Cuboid6(e.boundingBox).expand(e.getCollisionBorderSize)
                if (a2.toAABB.isVecInside(start.toVec3D))//Why doesnt Cuboid6 have this method!?
                {
                    eHit = e
                    edis = 0.0D
                }
                else
                {
                    val mop = RayTracer.instance.rayTraceCuboid(start, end, box)
                    if (mop != null)
                    {
                        val d = new Vector3(mop.hitVec).subtract(start).mag
                        if (d < edis || edis == 0.0D)
                        {
                            eHit = e
                            edis = d
                        }
                    }
                }
            }
        }
        eHit
    }
}

object TileBlockPlacer
{
    var fake = WeakReference[EntityPlayer](null)
    def fakePlayer = fake.get.orNull

    var angles = Seq(
        (-90F, 0F),
        (90F, 0F),
        (0F, 0F),
        (0F, 180F),
        (0F, 270F),
        (0F, 90F)
    )

    val positions = Seq(
        new Vector3(0.5D, -1.1D+0.51D, 0.5D),
        new Vector3(0.5D, -1.1D-0.51D, 0.5D),
        new Vector3(0.5D, -1.1D, 0.5D+0.51D),
        new Vector3(0.5D, -1.1D, 0.5D-0.51D),
        new Vector3(0.5D+0.51D, -1.1D, 0.5D),
        new Vector3(0.5D-0.51D, -1.1D, 0.5D)
    )
}

class GuiBlockPlacer(c:Container) extends WidgetGui(c, 176, 168)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        PRResources.guiBlockPlacer.bind()
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, 176, 168)
        GuiDraw.drawString("Block Placer", 4, 4, Colors.GREY.argb, false)
    }
}

object GuiBlockPlacer extends TGuiBuilder
{
    override def getID = ExpansionProxy.blockPlacerGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val t = WorldLib.getTileEntity(player.worldObj, data.readCoord(), classOf[TileBlockPlacer])
        if (t != null) new GuiBlockPlacer(t.createContainer(player))
        else null
    }
}

object RenderBlockPlacer extends TInstancedBlockRender with TCubeMapRender
{
    var bottom:IIcon = _
    var side1A:IIcon = _
    var side2A:IIcon = _
    var topA:IIcon = _
    var side1B:IIcon = _
    var side2B:IIcon = _
    var topB:IIcon = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _

    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TActiveDevice])
        if (te != null) (te.side, te.rotation, if (te.active || te.powered) iconT2 else iconT1)
        else (0, 0, iconT1)
    }

    override def getInvData = (0, 0, iconT1)

    override def getIcon(s:Int, meta:Int) = s match
    {
        case 0 => bottom
        case 1 => topA
        case 2 => side1A
        case 3 => side1A
        case 4 => side2A
        case 5 => side2A
        case _ => bottom
    }

    override def registerIcons(reg:IIconRegister)
    {
        def register(s:String) = reg.registerIcon(s"projectred:machines/placer/$s")
        bottom = register("bottom")

        side1A = register("side1A")
        side2A = register("side2A")
        topA = register("topA")

        side1B = register("side1B")
        side2B = register("side2B")
        topB = register("topB")

        iconT1 = new MultiIconTransformation(bottom, topA, side1A, side1A, side2A, side2A)
        iconT2 = new MultiIconTransformation(bottom, topB, side1B, side1B, side2B, side2B)
    }
}