package mrtjp.projectred.core;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import net.minecraft.block.Block;
import net.minecraft.entity.item.EntityItem;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTBase;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Vec3;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TileMultipart;

public class BasicUtils {
    
    public static boolean isServer(World world) {
        if (world != null)
            return !world.isRemote;
        return false;
    }

    public static boolean isClient(World world) {
        if (world != null)
            return world.isRemote;
        return false;
    }

    public static void dropItem(World world, int x, int y, int z, ItemStack itemStack) {
        if (!isClient(world)) {
            double var5 = 0.7D;
            double var7 = (double) world.rand.nextFloat() * var5 + (1.0D - var5) * 0.5D;
            double var9 = (double) world.rand.nextFloat() * var5 + (1.0D - var5) * 0.5D;
            double var11 = (double) world.rand.nextFloat() * var5 + (1.0D - var5) * 0.5D;
            EntityItem var13 = new EntityItem(world, (double) x + var7, (double) y + var9, (double) z + var11, itemStack);
            var13.delayBeforeCanPickup = 10;
            world.spawnEntityInWorld(var13);
        }
    }

    public static void dropItemFromLocation(World w, ItemStack is, boolean violent, EntityPlayer player, int to_side, int tickDelay, BlockCoord coord) {
        if (w.isRemote)
            return;
        if ((is == null) || (is.stackSize == 0))
            return;
        if (player == null)
            to_side = -1;
        double mult = 0.02D;
        if (violent)
            mult = 0.2D;
        Vec3 pos = w.getWorldVec3Pool().getVecFromPool(coord.x + 0.5D, coord.y + 0.5D, coord.z + 0.5D);
        Vec3 vel = w.getWorldVec3Pool().getVecFromPool(0.0D, 0.0D, 0.0D);
        if (to_side != -1) {
            ForgeDirection dir = ForgeDirection.getOrientation(to_side);

            double d = 0.75D;
            pos.xCoord += dir.offsetX * d;
            pos.yCoord += dir.offsetY * d;
            pos.zCoord += dir.offsetZ * d;
            vel.xCoord = dir.offsetX;
            vel.yCoord = dir.offsetY;
            vel.zCoord = dir.offsetZ;
        } else if (player != null) {
            Vec3 vec = Vec3.createVectorHelper(player.posX - coord.x, player.posY - coord.y, player.posZ - coord.z);

            vec = vec.normalize();
            vel = vec;
            double d = 0.25D;

            pos.xCoord += vec.xCoord * d;
            pos.yCoord += vec.yCoord * d;
            pos.zCoord += vec.zCoord * d;
        } else {
            vel.xCoord = w.rand.nextGaussian();
            vel.yCoord = w.rand.nextGaussian();
            vel.zCoord = w.rand.nextGaussian();
        }
        EntityItem ent = new EntityItem(w, pos.xCoord, pos.yCoord, pos.zCoord, is);
        ent.motionX = (vel.xCoord * mult);
        ent.motionY = (vel.yCoord * mult);
        ent.motionZ = (vel.zCoord * mult);
        ent.delayBeforeCanPickup = tickDelay;
        w.spawnEntityInWorld(ent);
    }


    public static <T> T getTileEntity(IBlockAccess access, BlockCoord coords, Class<T> clazz) {
        TileEntity te = access.getBlockTileEntity(coords.x, coords.y, coords.z);
        return !clazz.isInstance(te) ? null : (T)te;
    }
    
    /**
     * Faster than class.isInstance
     */
    public static TileMultipart getMultipartTile(IBlockAccess access, BlockCoord pos) {
        TileEntity te = access.getBlockTileEntity(pos.x, pos.y, pos.z);
        return te instanceof TileMultipart ? (TileMultipart)te : null;
    }

    public static boolean areStacksTheSame(ItemStack is1, ItemStack is2) {
        if (is1 == null || is2 == null)
            return false;
        return is1.itemID == is2.itemID && is1.getItemDamage() == is2.getItemDamage();
    }

    public static void markBlockDirty(World w, int x, int y, int z) {
        if (w.blockExists(x, y, z))
            w.getChunkFromBlockCoords(x, z).setChunkModified();
    }
    
    public static void updateIndirectNeighbors(World w, int x, int y, int z, int id) {
        if ((w.scheduledUpdatesAreImmediate) || (isClient(w)))
            return;
        for (int a = -3; a <= 3; a++)
            for (int b = -3; b <= 3; b++)
                for (int c = -3; c <= 3; c++) {
                    int md = a < 0 ? -a : a;
                    md += (b < 0 ? -b : b);
                    md += (c < 0 ? -c : c);
                    if (md <= 3)
                        notifyBlock(w, x + a, y + b, z + c, id);
                }
    }

    public static void notifyBlock(World w, int x, int y, int z, int id) {
        Block block = Block.blocksList[w.getBlockId(x, y, z)];
        if (block != null)
            block.onNeighborBlockChange(w, x, y, z, id);
    }
    
    public static void writeNBTToData(NBTBase nbt, DataOutputStream data) throws IOException {
        NBTBase.writeNamedTag(nbt, data);
    }

    public static NBTBase readNBTFromData(DataInputStream data) throws IOException {
        return NBTBase.readNamedTag(data);
    }

}
