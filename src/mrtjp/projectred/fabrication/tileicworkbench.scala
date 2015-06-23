/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.packet.PacketCustom
import codechicken.lib.render.uv.MultiIconTransformation
import codechicken.lib.vec.{Rotation, Vector3}
import mrtjp.core.block.{InstancedBlock, InstancedBlockTile, TInstancedBlockRender, TTileOrient}
import mrtjp.core.render.TCubeMapRender
import mrtjp.core.world.WorldLib
import mrtjp.projectred.api.IScrewdriver
import mrtjp.projectred.fabrication.ItemICBlueprint._
import mrtjp.projectred.{ProjectRedFabrication, ProjectRedIntegration}
import net.minecraft.block.material.Material
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.item.EntityItem
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.IIcon
import net.minecraft.world.IBlockAccess

class BlockICMachine extends InstancedBlock("projectred.integration.icblock", Material.iron)
{
    setHardness(2)
    setCreativeTab(ProjectRedIntegration.tabIntegration2)
}

abstract class TileICMachine extends InstancedBlockTile with TTileOrient
{
    override def getBlock = ProjectRedFabrication.icBlock

    override def onBlockPlaced(s:Int, meta:Int, player:EntityPlayer, stack:ItemStack, hit:Vector3)
    {
        setSide(0)
        setRotation((Rotation.getSidedRotation(player, 1)+2)%4)
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeByte(orientation)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        orientation = in.readByte
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setByte("rot", orientation)
    }

    override def load(tag:NBTTagCompound)
    {
        orientation = tag.getByte("rot")
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 1 =>
            orientation = in.readByte()
            markRender()
        case _ => super.read(in, key)
    }

    override def onBlockActivated(player:EntityPlayer, side:Int):Boolean =
    {
        val held = player.getHeldItem
        if (doesRotate && held != null && held.getItem.isInstanceOf[IScrewdriver] && held.getItem.asInstanceOf[IScrewdriver].canUse(player, held))
        {
            if (world.isRemote) return true
            setRotation((rotation+1)%4)
            sendOrientUpdate()
            held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, held)
            true
        }
        else false
    }

    def doesRotate = true

    def sendOrientUpdate()
    {
        writeStream(1).writeByte(orientation).sendToChunk()
    }
}

class TileICWorkbench extends TileICMachine with NetWorldCircuit
{
    val circuit = new IntegratedCircuit
    circuit.network = this

    var hasBP = false

    override def getIC = circuit
    override def getWorld = world
    override def isRemote = world.isRemote
    override def createPartStream() = writeStream(3)
    override def sendPartStream(out:PacketCustom){out.sendToChunk()} //TODO send only to watchers
    override def createICStream() = writeStream(4)
    override def sendICStream(out:PacketCustom){if (world.isRemote) out.sendToServer() else out.sendToChunk()} //TODO send only to watchers

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        val ictag = new NBTTagCompound
        circuit.save(ictag)
        tag.setTag("ictag", ictag)
        tag.setBoolean("bp", hasBP)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        circuit.load(tag.getCompoundTag("ictag"))
        hasBP = tag.getBoolean("bp")
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeBoolean(hasBP)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        hasBP = in.readBoolean()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 1 =>
            hasBP = in.readBoolean()
            markRender()
        case 2 => circuit.readDesc(in)
        case 3 => readPartStream(in)
        case 4 => readICStream(in)
        case 5 =>
            if (!hasBP) new IntegratedCircuit().readDesc(in)
            else {circuit.readDesc(in); sendICDesc()}
        case _ => super.read(in, key)
    }

    private def sendICDesc()
    {
        val out = writeStream(2)
        circuit.writeDesc(out)
        out.sendToChunk() //TODO send only to gui open players
    }

    private def sendICDesc(players:EntityPlayer*)
    {
        val out = writeStream(2)
        circuit.writeDesc(out)
        for (p <- players)
            out.sendToPlayer(p)
    }

    private def sendHasBPUpdate()
    {
        writeStream(1).writeBoolean(hasBP).sendToChunk()
    }

    override def update()
    {
        super.update()
        flushICStream()
        flushPartStream()
        circuit.tick()
    }
    override def updateClient()
    {
        super.updateClient()
        flushICStream() //ic stream is bi-directional
    }

    override def onBlockActivated(player:EntityPlayer, side:Int):Boolean =
    {
        if (super.onBlockActivated(player, side)) return true
        if (!world.isRemote)
        {
            import ItemICBlueprint._
            if (!hasBP && player.getHeldItem != null && player.getHeldItem.getItem.isInstanceOf[ItemICBlueprint])
            {
                val stack = player.getHeldItem
                if (hasICInside(stack))
                {
                    loadIC(circuit, stack)
                    sendICDesc()
                }
                stack.stackSize -= 1
                hasBP = true
                sendHasBPUpdate()
            }
            else if (hasBP && player.isSneaking)
            {
                val stack = new ItemStack(ProjectRedFabrication.itemICBlueprint)
                if (circuit.nonEmpty)
                {
                    saveIC(circuit, stack)
                    circuit.clear()
                    sendICDesc()
                }
                //WorldLib.dropItem(world, position.offset(1), stack)
                val p = position.offset(1)
                val item = new EntityItem(world, p.x+0.5, p.y+0.20, p.z+0.5, stack)
                item.delayBeforeCanPickup = 10
                item.motionX = 0
                item.motionY = 0.15
                item.motionZ = 0
                world.spawnEntityInWorld(item)
                hasBP = false
                sendHasBPUpdate()
            }
            else
            {
                GuiICWorkbench.open(player, null, _.writeCoord(x, y, z))
                sendICDesc(player)
            }
        }
        true
    }

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        if (hasBP)
        {
            val stack = new ItemStack(ProjectRedFabrication.itemICBlueprint)
            if (circuit.nonEmpty) saveIC(circuit, stack)
            WorldLib.dropItem(world, x, y, z, stack)
        }
    }

    override def doesRotate = false
}

object RenderICWorkbench extends TInstancedBlockRender with TCubeMapRender
{
    var bottom:IIcon = _
    var side1:IIcon = _
    var side2:IIcon = _
    var sidebp1:IIcon = _
    var sidebp2:IIcon = _
    var top:IIcon = _
    var topBP:IIcon = _

    var iconT:MultiIconTransformation = _
    var iconTBP:MultiIconTransformation = _

    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TileICWorkbench])

        (0, 0, if (te.hasBP) iconTBP else iconT)
    }

    override def getInvData = (0, 0, iconTBP)

    override def getIcon(s:Int, meta:Int) = iconTBP.icons(s)

    override def registerIcons(reg:IIconRegister)
    {
        def register(s:String) = reg.registerIcon("projectred:circuits/icprojectbench/"+s)

        bottom = register("bottom")
        top = register("top")
        topBP = register("topbp")
        side1 = register("side1")
        side2 = register("side2")
        sidebp1 = register("sidebp1")
        sidebp2 = register("sidebp2")

        iconT = new MultiIconTransformation(bottom, top, side1, side1, side2, side2)
        iconTBP = new MultiIconTransformation(bottom, topBP, sidebp1, sidebp1, sidebp2, sidebp2)
    }
}