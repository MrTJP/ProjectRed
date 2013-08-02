package mrtjp.projectred.items;

import mrtjp.projectred.crafting.ProjectRedTabs;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.minecraftforge.common.IPlantable;
import net.minecraftforge.common.IShearable;

/**
 * The sickle is a tool that allows the player to harvest leaves and crops in a
 * 3x3x3 area around the block where the player destroyed
 *
 * @author Shirkit
 */
public class ItemSickle extends Item {

    public static final int radiusElse = 1;
    public static final int radiusLeaves = 2;

    public ItemSickle(int id) {
        super(id);
        setMaxDamage(64);
        setMaxStackSize(1);
        setCreativeTab(ProjectRedTabs.tabTools);
        setUnlocalizedName("projectred.items.sickle");
    }

    @Override
    public boolean onBlockDestroyed(ItemStack stack, World world, int blockId, int blockX, int blockY, int blockZ, EntityLivingBase entity) {
        boolean work = false;

        if (!(entity instanceof EntityPlayer)) {
            return false;
        }
        EntityPlayer player = (EntityPlayer) entity;

        Block source = Block.blocksList[blockId];
        if (!world.isRemote && canHit(blockId)) {
            int radius = radiusElse;
            if (source.isLeaves(world, blockX, blockY, blockZ)) {
                radius = radiusLeaves;
            }
            for (int x = -radius; x <= radius; x++) {
                for (int y = -radius; y <= radius; y++) {
                    for (int z = -radius; z <= radius; z++) {
                        int id = world.getBlockId(blockX + x, blockY + y, blockZ + z);
                        if (canHit(id)) {
                            Block type = Block.blocksList[id];
                            int meta = world.getBlockMetadata(blockX + x, blockY + y, blockZ + z);
                            if (type.canHarvestBlock(player, meta)) {
                                // We set to AIR to prevent all the cut sounds from playing (instead of destroying the block)
                                type.harvestBlock(world, player, blockX + x, blockY + y, blockZ + z, meta);
                                world.setBlock(blockX + x, blockY + y, blockZ + z, 0);
                            }
                            work = true;
                        }
                    }

                }
            }

            if (work) {
                stack.damageItem(1, entity);
            }
        }
        return work;
    }

    @Override
    public boolean canHarvestBlock(Block par1Block) {
        return canHit(par1Block.blockID);
    }

    private boolean canHit(int id) {
        return Block.blocksList[id] instanceof IShearable || Block.blocksList[id] instanceof IPlantable;
    }

    @Override
    public float getStrVsBlock(ItemStack par1ItemStack, Block par2Block) {
        if (canHarvestBlock(par2Block)) {
            return 15f;
        }
        return super.getStrVsBlock(par1ItemStack, par2Block);
    }

    @Override
    public void registerIcons(IconRegister par1IconRegister) {
        this.itemIcon = par1IconRegister.registerIcon("projectred:sickle");
    }
}