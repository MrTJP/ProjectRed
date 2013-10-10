package mrtjp.projectred.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemBlockMulti extends ItemBlock {
    HashMap names = new HashMap();
    ArrayList<Integer> valid = new ArrayList<Integer>();

    public ItemBlockMulti(int i) {
        super(i);
        setMaxDamage(0);
        setHasSubtypes(true);
    }

    @Override
    public int getMetadata(int i) {
        return i;
    }

    public void setMetaName(int dmg, String name) {
        names.put(Integer.valueOf(dmg), name);
        valid.add(Integer.valueOf(dmg));
    }

    @SideOnly(Side.CLIENT)
    @Override
    public void getSubItems(int id, CreativeTabs tab, List list) {
        for (Integer i : valid)
            list.add(new ItemStack(this, 1, i.intValue()));
    }

    @Override
    public boolean placeBlockAt(ItemStack stack, EntityPlayer player, World w, int x, int y, int z, int side, float hitX, float hitY, float hitZ, int meta) {
        if (!w.setBlock(x, y, z, itemID, meta, 3))
            return false;
        
        if (w.getBlockId(x, y, z) == itemID) {
            BlockMulti b = (BlockMulti) net.minecraft.block.Block.blocksList[itemID];
            b.onBlockPlacedBy(w, x, y, z, player, stack);
        }
        return true;
    }
}
