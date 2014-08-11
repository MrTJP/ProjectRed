package mrtjp.projectred.core.libmc

import net.minecraft.tileentity.TileEntity
import net.minecraft.world.{IBlockAccess, World}
import net.minecraft.entity.item.EntityItem
import net.minecraft.item.ItemStack
import net.minecraft.entity.player.EntityPlayer
import codechicken.lib.vec.{BlockCoord, Vector3}
import codechicken.multipart.{TMultiPart, TileMultipart}
import net.minecraft.block.Block

object PRLib
{
    def dropItem(w:World, bc:BlockCoord, stack:ItemStack)
    {
        dropItem(w, bc.x, bc.y, bc.z, stack)
    }
    def dropItem(w:World, x:Int, y:Int, z:Int, stack:ItemStack)
    {
        if (!w.isRemote)
        {
            val d = 0.7D
            val dx = w.rand.nextFloat*d+(1.0D-d)*0.5D
            val dy = w.rand.nextFloat*d+(1.0D-d)*0.5D
            val dz = w.rand.nextFloat*d+(1.0D-d)*0.5D
            val item = new EntityItem(w, x+dx, y+dy, z+dz, stack)
            item.delayBeforeCanPickup = 10
            w.spawnEntityInWorld(item)
        }
    }

    def dropTowardsPlayer(w:World, bc:BlockCoord, stack:ItemStack, p:EntityPlayer)
    {
        dropTowardsPlayer(w, bc.x, bc.y, bc.z, stack, p)
    }
    def dropTowardsPlayer(w:World, x:Int, y:Int, z:Int, stack:ItemStack, p:EntityPlayer)
    {
        if (!w.isRemote)
        {
            val vel = ~new Vector3(p.posX-x, p.posY-y, p.posZ-z)
            val pos = new Vector3(x+0.5, y+0.5, z+0.5)+(vel.copy*0.25)

            val item = new EntityItem(w, pos.x, pos.y, pos.z, stack)
            item.motionX = vel.x*0.02
            item.motionY = vel.y*0.02
            item.motionZ = vel.z*0.02
            item.delayBeforeCanPickup = 10
            w.spawnEntityInWorld(item)
        }
    }

    def getTileEntity[T](w:IBlockAccess, bc:BlockCoord, clazz:Class[T]):T =
        getTileEntity(w, bc.x, bc.y, bc.z, clazz)

    def getTileEntity[T](w:IBlockAccess, x:Int, y:Int, z:Int, clazz:Class[T]):T =
    {
        if (y < 0) null.asInstanceOf[T]
        else
        {
            val tile = w.getTileEntity(x, y, z)
            if (!clazz.isInstance(tile)) null.asInstanceOf[T] else tile.asInstanceOf[T]
        }
    }

    def getMultipartTile(w:IBlockAccess, bc:BlockCoord):TileMultipart = getMultipartTile(w, bc.x, bc.y, bc.z)
    def getMultipartTile(w:IBlockAccess, x:Int, y:Int, z:Int) = w.getTileEntity(x, y, z) match
    {
        case t:TileMultipart => t
        case _ => null
    }

    def getMultiPart(w:IBlockAccess, bc:BlockCoord, part:Int):TMultiPart = getMultiPart(w, bc.x, bc.y, bc.z, part)
    def getMultiPart(w:IBlockAccess, x:Int, y:Int, z:Int, part:Int) =
    {
        val t = getMultipartTile(w, x, y, z)
        if (t != null) t.partMap(part)
        else null
    }

    def bulkBlockUpdate(w:World, x:Int, y:Int, z:Int, bl:Block)
    {
        for (a <- -3 to 3) for (b <- -3 to 3) for (c <- -3 to 3)
        {
            val md = (if (a < 0) -a else a) + (if (b < 0) -b else b) + (if (c < 0) -c else c)
            if (md <= 3)
            {
                val block = w.getBlock(x+a, y+b, z+c)
                if (block != null) block.onNeighborBlockChange(w, x+a, y+b, z+c, bl)
            }
        }
    }
}
