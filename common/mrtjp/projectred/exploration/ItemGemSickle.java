package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import net.minecraft.block.Block;
import net.minecraft.block.BlockFlower;
import net.minecraft.block.BlockLeaves;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemTool;
import net.minecraft.world.World;
import net.minecraftforge.common.IPlantable;

public class ItemGemSickle extends ItemTool
{
    EnumSpecialTool tool;

    int radiusCrops = 2;
    int radiusLeaves = 1;

    public ItemGemSickle(int par1, EnumSpecialTool tool)
    {
        super(par1, 3, tool.material, new Block[0]);
        this.tool = tool;
        this.setUnlocalizedName("projectred.exploration." + tool.unlocal);
        this.setCreativeTab(ProjectRedExploration.tabExploration());
    }

    @Override
    public boolean getIsRepairable(ItemStack ist1, ItemStack ist2)
    {
        if (tool.repairStack.isItemEqual(ist2))
            return true;

        return super.getIsRepairable(ist1, ist2);
    }

    @Override
    public float getStrVsBlock(ItemStack ist, Block bl)
    {
        if (bl instanceof BlockLeaves)
            return this.efficiencyOnProperMaterial;
        return super.getStrVsBlock(ist, bl);
    }

    public boolean recurseLeaves(ItemStack stack, World w, int x, int y, int z, EntityPlayer player)
    {
        boolean used = false;
        for (int i = -radiusLeaves; i <= radiusLeaves; i++)
            for (int j = -radiusLeaves; j <= radiusLeaves; j++)
                for (int k = -radiusLeaves; k <= radiusLeaves; k++)
                {
                    int localX = x + i;
                    int localY = y + j;
                    int localZ = z + k;
                    int id = w.getBlockId(localX, localY, localZ);
                    int meta = w.getBlockMetadata(localX, localY, localZ);
                    Block localBlock = Block.blocksList[id];
                    if (localBlock != null && (localBlock.isLeaves(w, localX, localY, localZ) || localBlock instanceof BlockLeaves))
                    {
                        if (localBlock.canHarvestBlock(player, meta))
                            localBlock.harvestBlock(w, player, localX, localY, localZ, meta);
                        w.setBlock(localX, localY, localZ, 0, 0, 3);
                        used = true;
                    }
                }
        if (used)
            stack.damageItem(1, player);
        return used;
    }

    public boolean recurseCrops(ItemStack stack, World w, int x, int y, int z, EntityPlayer player)
    {
        boolean used = false;
        for (int i = -radiusCrops; i <= radiusCrops; i++)
            for (int j = -radiusCrops; j <= radiusCrops; j++)
            {
                int localX = x + i;
                int localY = y;
                int localZ = z + j;
                int id = w.getBlockId(localX, localY, localZ);
                int meta = w.getBlockMetadata(localX, localY, localZ);
                Block localBlock = Block.blocksList[id];
                if (localBlock != null && (localBlock instanceof BlockFlower || localBlock instanceof IPlantable))
                {
                    if (localBlock.canHarvestBlock(player, meta))
                        localBlock.harvestBlock(w, player, localX, localY, localZ, meta);
                    w.setBlock(localX, localY, localZ, 0, 0, 3);
                    used = true;
                }
            }
        if (used)
            stack.damageItem(1, player);
        return used;
    }

    @Override
    public boolean onBlockDestroyed(ItemStack stack, World world, int blockID, int x, int y, int z, EntityLivingBase entity)
    {
        EntityPlayer player;
        if (entity instanceof EntityPlayer)
            player = (EntityPlayer) entity;
        else
            return false;

        Block b = Block.blocksList[blockID];
        if (b != null)
        {
            if (b.isLeaves(world, x, y, z))
                return recurseLeaves(stack, world, x, y, z, player);

            if (b instanceof BlockFlower)
                return recurseCrops(stack, world, x, y, z, player);
        }
        return super.onBlockDestroyed(stack, world, blockID, x, y, z, entity);
    }

    @Override
    public void registerIcons(IconRegister reg)
    {
        this.itemIcon = reg.registerIcon("projectred:gemtools/" + tool.unlocal);
    }
}
