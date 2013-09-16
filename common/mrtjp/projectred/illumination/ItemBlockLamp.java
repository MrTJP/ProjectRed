package mrtjp.projectred.illumination;

import net.minecraft.block.Block;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.world.World;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemBlockLamp extends ItemBlock {

    public ItemBlockLamp(int par1) {
        super(par1);
        setHasSubtypes(true);
    }

    public int getMetadata(int md) {
        return md;
    }

    @Override
    public String getUnlocalizedName(ItemStack item) {
        String s = item.getItemDamage()>15?".inv":"";
        int s2 = item.getItemDamage()>15?item.getItemDamage()-16:item.getItemDamage();
        return Block.blocksList[getBlockID()].getUnlocalizedName()+s+"|"+s2;
    }

    @Override
    public boolean placeBlockAt(ItemStack stack, EntityPlayer player, World world, int x, int y, int z, int side, float hitX, float hitY, float hitZ, int metadata) {
        int meta = stack.getItemDamage();
        if (meta < 0 || meta >= 32)
            return false;

        if (!super.placeBlockAt(stack, player, world, x, y, z, side, hitX, hitY, hitZ, metadata))
            return false;

        if (world.getBlockId(x, y, z) == itemID) {
            TileLamp lamp = new TileLamp();
            lamp.prepairPlacement(meta > 15, meta>15?meta-16:meta);
            world.setBlockTileEntity(x, y, z, lamp);
        }
        return true;
    }

    @SideOnly(Side.CLIENT)
    @Override
    public Icon getIconFromDamage(int meta) {
        if (meta > 15)
            return BlockLamp.onIcons[meta - 16];
        else
            return BlockLamp.offIcons[meta];
    }

}
