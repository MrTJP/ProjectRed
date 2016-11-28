/*
 * Copyright (c) 2016.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.transportation

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.CuboidRayTraceResult
import mrtjp.projectred.api.IScrewdriver
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.EnumHand
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class NetworkValvePipePart extends AbstractNetPipe with TNetworkSubsystem with TPipeTravelConditions
{
    /**
     * 0000 EEWW SSNN UUDD
     * -> OI
     *
     * 0x0 - None
     * 0x1 - Input only
     * 0x2 - Output only
     * 0x3 - Input and Output
     */
    var pathMatrix = 0x0FFF

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setShort("ioflag", pathMatrix.toShort)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        pathMatrix = tag.getShort("ioflag")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeShort(pathMatrix)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        pathMatrix = packet.readShort()
    }

    def sendIOFlagsUpdate()
    {
        getWriteStreamOf(6).writeShort(pathMatrix)
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 6 =>
            pathMatrix = packet.readShort()
            tile.markRender()
        case _ => super.read(packet, key)
    }

    override def chooseRandomDestination(r:NetworkPayload, mask:Int) =
    {
        var moves = Seq[Int]()
        for (s <- 0 until 6)
            if((connMap&1<<s) != 0 && s != (r.input^1) && (mask&1<<s) == 0)
                if (((pathMatrix>>(s*2))&2) != 0) moves :+= s
        if (moves.isEmpty) r.output = r.input^1
        else r.output = moves(world.rand.nextInt(moves.size))
    }


    override def activate(player:EntityPlayer, hit:CuboidRayTraceResult, item:ItemStack, hand:EnumHand):Boolean =
    {
        if (item != null && item.getItem.isInstanceOf[IScrewdriver] &&
                item.getItem.asInstanceOf[IScrewdriver].canUse(player, item) &&
                ((0 until 6) contains hit.cuboid6.data.asInstanceOf[Int]) && !player.isSneaking) {
            if (!world.isRemote) {
                val side = hit.cuboid6.data.asInstanceOf[Int]
                val mode = (pathMatrix>>(side*2))&3
                val newMode = (mode+1)%3+1 //cycle 1,2,3
                pathMatrix = pathMatrix& ~(3<<(side*2))|(newMode<<(side*2))
                sendIOFlagsUpdate()
            }
            return true
        }

        super.activate(player, hit, item, hand)
    }

    /**
     * 00FT
     * T - can travel to
     * F - can come from
     */
    override def getPathFlags(input:Int, output:Int) =
    {
        var flags = 0
        val to1 = (pathMatrix>>(input*2))&3
        val to2 = (pathMatrix>>(output*2))&3
        if ((to1&1|to2&2) == 0x3) flags |= 0x1
        if ((to2&1|to1&2) == 0x3) flags |= 0x2
        flags
    }

    @SideOnly(Side.CLIENT)
    override def getIcon(side:Int) =
    {
        PipeDefs.NETWORKVALVE.sprites((pathMatrix>>(side*2))&3)
    }
}

class NetworkLatencyPipePart extends AbstractNetPipe with TNetworkSubsystem with TPipeTravelConditions
{
    override def getPathWeight = 64
}