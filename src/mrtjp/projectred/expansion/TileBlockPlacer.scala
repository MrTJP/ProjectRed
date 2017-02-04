/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import java.util.UUID

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.MCDataInput
import codechicken.lib.gui.GuiDraw
import codechicken.lib.model.blockbakery.SimpleBlockRenderer
import codechicken.lib.raytracer.{IndexedCuboid6, RayTracer}
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import codechicken.lib.vec.{Cuboid6, Vector3}
import codechicken.multipart.IRedstoneConnector
import com.mojang.authlib.GameProfile
import mrtjp.core.gui._
import mrtjp.core.inventory.TInventory
import mrtjp.core.vec.Point
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.expansion.TileBlockPlacer._
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.{Entity, EntityLivingBase}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.math.{BlockPos, Vec3d}
import net.minecraft.util.{EnumActionResult, EnumFacing, EnumHand, ResourceLocation}
import net.minecraft.world.WorldServer
import net.minecraftforge.common.ForgeHooks
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.common.util.FakePlayerFactory
import net.minecraftforge.fml.common.eventhandler.Event
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.ref.WeakReference

class TileBlockPlacer extends TileMachine with TActiveDevice with TInventory with IRedstoneConnector
{
    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        loadInv(tag)
    }

    override def markDirty()
    {
        super.markDirty()
    }

    override def getDisplayName = super.getDisplayName
    override def getBlock = ProjectRedExpansion.machine2
    override def doesRotate = false
    override def doesOrient = true

    override def size = 9
    override def name = "block_placer"

    def reloadPlayer()
    {
        if (fakePlayer == null)
            fake = WeakReference(FakePlayerFactory.get(world.asInstanceOf[WorldServer],
                new GameProfile(UUID.randomUUID(), "[PR_FAKE]")))

        val pos = new Vector3(x, y, z).add(positions(side))

        val (pitch, yaw) = angles(side)
        fakePlayer.setLocationAndAngles(pos.x, pos.y, pos.z, yaw, pitch)
    }

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(world, getPos)
    }

    override def onBlockActivated(player:EntityPlayer, actside:Int):Boolean =
    {
        if (super.onBlockActivated(player, actside)) return true

        if (!world.isRemote)
            GuiBlockPlacer.open(player, createContainer(player), _.writePos(getPos))
        true
    }

    def createContainer(player:EntityPlayer) =
        new ContainerBlockPlacer(player, this)

    override def onActivate()
    {
        if (world.isRemote) return
        reloadPlayer()
        val upos = getPos.offset(EnumFacing.VALUES(side^1))
        copyInvToPlayer()
        for (i <- 0 until 9)
        {
            val stack = getStackInSlot(i)
            if (stack != null && tryUseItem(stack, upos, i))
            {
                if (fakePlayer.isHandActive) fakePlayer.stopActiveHand()
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
    //TODO
    //FIXME This all need a complete rewrite due to the new interaction system.
    def tryUseItem(stack:ItemStack, pos:BlockPos, slot:Int) =
    {
        fakePlayer.inventory.currentItem = slot

        try {
            tryRightClick(stack, pos, slot) || tryEntityClick(stack, pos)
        } catch {
            case e:Throwable => false
        }
    }

    def tryRightClick(stack:ItemStack, pos:BlockPos, slot:Int):Boolean =
    {
        def tryUse(pos:BlockPos):Boolean =
        {
            val event = ForgeHooks.onRightClickBlock(fakePlayer, EnumHand.MAIN_HAND, stack, pos, EnumFacing.UP, new Vec3d(0.5, 0.5, 0.5))
            if (event.isCanceled || event.getUseBlock == Event.Result.DENY) return false

            import EnumActionResult._

            stack.getItem.onItemUseFirst(stack, fakePlayer, world, pos,
                EnumFacing.UP, 0.5F, 0.5f, 0.5F, EnumHand.MAIN_HAND) match {
                case FAIL => return false
                case SUCCESS => return true
                case PASS =>
            }

            stack.onItemUse(fakePlayer, world, pos, EnumHand.MAIN_HAND,
                EnumFacing.UP, 0.5f, 0.5f, 0.5f) match {
                case FAIL => return false
                case SUCCESS => return true
                case PASS =>
            }

            false
        }

        if (tryUse(pos)) return true
        if (tryUse(pos.down)) return true

//        val size = stack.stackSize
//        val newStack = stack.useItemRightClick(world, fakePlayer)
//        if (stack != newStack || (newStack != null && newStack.stackSize != size))
//        {
//            fakePlayer.inventory.setInventorySlotContents(slot, newStack)
//            return true
//        }

        false
    }

    def tryEntityClick(stack:ItemStack, pos:BlockPos):Boolean =
    {
        val start = Vector3.fromBlockPosCenter(pos)
        val end = Vector3.fromBlockPosCenter(pos.offset(EnumFacing.VALUES(side^1), 2))
        val e = traceEntityHits(start, end)
        e != null && useOnEntity(stack, e)
    }

    def useOnEntity(stack:ItemStack, e:Entity):Boolean =
    {
        import EnumActionResult._
        e.applyPlayerInteraction(fakePlayer, new Vec3d(0, 0, 0), stack, EnumHand.MAIN_HAND) match {
            case FAIL => return false
            case SUCCESS => return true
            case PASS =>
        }

        if (e.isInstanceOf[EntityLivingBase])
            if(stack.interactWithEntity(fakePlayer, e.asInstanceOf[EntityLivingBase], EnumHand.MAIN_HAND))
                return true
        false
    }

    def traceEntityHits(start:Vector3, end:Vector3) =
    {
        val box = new Cuboid6(0, 1, 0, 1, 3.5, 1).apply(rotationT).add(pos)
        val elist = world.getEntitiesWithinAABBExcludingEntity(fakePlayer, box.aabb)

        import scala.collection.JavaConversions._
        val eBoxes = elist.zipWithIndex.filter(_._1.canBeCollidedWith).map { pair =>
            new IndexedCuboid6(pair._2, new Cuboid6(pair._1.getEntityBoundingBox)
                    .expand(pair._1.getCollisionBorderSize))
        }

        val hit = RayTracer.rayTraceCuboidsClosest(start, end, eBoxes, pos)
        if (hit != null)
            elist(hit.cuboid6.data.asInstanceOf[Int])
        else
            null

//        var eHit:Entity = null
//        var edis = Double.MaxValue

//        for (i <- 0 until elist.size())
//        {
//            val e = elist.get(i)
//            if (e.canBeCollidedWith)
//            {
//                val a2 = new IndexedCuboid6(0, new Cuboid6(e.getEntityBoundingBox).expand(e.getCollisionBorderSize))
//                if (a2.contains(start))//Why doesnt Cuboid6 have this method!?
//                {
//                    eHit = e
//                    edis = 0.0D
//                }
//                else
//                {
//                    val mop = RayTracer.instance.rayTraceCuboid(start, end, a2)
//                    RayTracer.rayTrace(pos, start, end, a2)
//                    if (mop != null)
//                    {
//                        val d = new Vector3(mop.hitVec).subtract(start).mag
//                        if (d < edis)
//                        {
//                            eHit = e
//                            edis = d
//                        }
//                    }
//                }
//            }
//        }
//        eHit
    }

    override def getConnectionMask(side:Int) = if ((side^1) == this.side) 0 else 0x1F
    override def weakPowerLevel(side:Int, mask:Int) = 0
}

object TileBlockPlacer
{
    var fake = WeakReference[EntityPlayer](null)
    def fakePlayer = fake.get.orNull

    val angles = Seq(
        (-90F, 0F),
        (90F, 0F),
        (0F, 0F),
        (0F, 180F),
        (0F, 270F),
        (0F, 90F)
    )

    val positions = Seq(
        new Vector3(0.5D,       -1.12D+0.60D, 0.5D),
        new Vector3(0.5D,       -1.12D-0.52D, 0.5D),
        new Vector3(0.5D,       -1.12D,       0.5D+0.52D),
        new Vector3(0.5D,       -1.12D,       0.5D-0.52D),
        new Vector3(0.5D+0.52D, -1.12D,       0.5D),
        new Vector3(0.5D-0.52D, -1.12D,       0.5D)
    )
}

class ContainerBlockPlacer(p:EntityPlayer, tile:TileBlockPlacer) extends NodeContainer
{
    {
        var s = 0
        for ((x, y) <- GuiLib.createSlotGrid(62, 18, 3, 3, 0, 0))
        {
            addSlotToContainer(new Slot3(tile, s, x, y))
            s += 1
        }
        addPlayerInv(p, 8, 86)
    }
}

class GuiBlockPlacer(c:ContainerBlockPlacer) extends NodeGui(c, 176, 168)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        TextureUtils.changeTexture(GuiBlockPlacer.background)
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, 176, 168)
        GuiDraw.drawString("Block Placer", 8, 6, EnumColour.GRAY.argb, false)
        GuiDraw.drawString("Inventory", 8, 75, EnumColour.GRAY.argb, false)
    }
}

object GuiBlockPlacer extends TGuiFactory
{
    val background = new ResourceLocation("projectred", "textures/gui/placer.png")

    override def getID = ExpansionProxy.blockPlacerGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val t = player.worldObj.getTileEntity(data.readPos()) match {
            case tile:TileBlockPlacer => tile
            case _ => null
        }
        if (t != null) new GuiBlockPlacer(t.createContainer(player))
        else null
    }
}

object RenderBlockPlacer extends SimpleBlockRenderer
{
    import java.lang.{Boolean => JBool, Integer => JInt}

    import mrtjp.core.util.CCLConversions._
    import mrtjp.projectred.expansion.BlockProperties._

    var bottom:TextureAtlasSprite = _
    var side1A:TextureAtlasSprite = _
    var side2A:TextureAtlasSprite = _
    var topA:TextureAtlasSprite = _
    var side1B:TextureAtlasSprite = _
    var side2B:TextureAtlasSprite = _
    var topB:TextureAtlasSprite = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _

    override def handleState(state: IExtendedBlockState, tileEntity: TileEntity): IExtendedBlockState = tileEntity match {
        case t: TActiveDevice => {
            var s = state
            s = s.withProperty(UNLISTED_SIDE_PROPERTY, t.side.asInstanceOf[JInt])
            s = s.withProperty(UNLISTED_ROTATION_PROPERTY, t.rotation.asInstanceOf[JInt])
            s = s.withProperty(UNLISTED_ACTIVE_PROPERTY, t.active.asInstanceOf[JBool])
            s.withProperty(UNLISTED_POWERED_PROPERTY, t.powered.asInstanceOf[JBool])
        }
        case _ => state
    }

    override def getWorldTransforms(state: IExtendedBlockState) = {
        val side = state.getValue(UNLISTED_SIDE_PROPERTY)
        val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
        val active = state.getValue(UNLISTED_ACTIVE_PROPERTY).asInstanceOf[Boolean]
        val powered = state.getValue(UNLISTED_POWERED_PROPERTY).asInstanceOf[Boolean]
        createTriple(side, rotation, if (active || powered) iconT2 else iconT1)
    }

    override def getItemTransforms(stack: ItemStack) = createTriple(0, 0, iconT1)
    override def shouldCull() = true

    def getIcon(s:Int, meta:Int) = s match
    {
        case 0 => bottom
        case 1 => topA
        case 2 => side1A
        case 3 => side1A
        case 4 => side2A
        case 5 => side2A
        case _ => bottom
    }

    override def registerIcons(reg:TextureMap)
    {
        def register(s:String) = reg.registerSprite(new ResourceLocation(s"projectred:blocks/mechanical/placer/$s"))
        bottom = register("bottom")

        side1A = register("side1a")
        side2A = register("side2a")
        topA = register("topa")

        side1B = register("side1b")
        side2B = register("side2b")
        topB = register("topb")

        iconT1 = new MultiIconTransformation(bottom, topA, side1A, side1A, side2A, side2A)
        iconT2 = new MultiIconTransformation(bottom, topB, side1B, side1B, side2B, side2B)
    }
}
