package mrtjp.projectred.exploration;

import java.util.List;
import java.util.Random;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.exploration.BlockStainedLeaf.EnumDyeTrees;
import net.minecraft.block.Block;
import net.minecraft.block.BlockSapling;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.IPlantable;
import codechicken.lib.vec.BlockCoord;

public class BlockStainedSapling extends BlockSapling {

    private Icon icon;

    public BlockStainedSapling(int par1) {
        super(par1);
        setStepSound(Block.soundGrassFootstep);
        setUnlocalizedName("projectred.exploration.dyesapling");
        setCreativeTab(ProjectRedExploration.tabExploration);
    }

    @Override
    public void growTree(World w, int x, int y, int z, Random r) {
        int saplingMeta = w.getBlockMetadata(x, y, z);
        GeneratorCustomTree gen = new GeneratorCustomTree(true, 5, Block.wood.blockID, 0, ProjectRedExploration.blockStainedLeaf.blockID, saplingMeta, -1, -1);
        w.setBlock(x, y, z, 0, 0, 3);
        if (!gen.generate(w, r, x, y, z))
            w.setBlock(x, y, z, ProjectRedExploration.blockStainedSapling.blockID, saplingMeta, 3);
    }

    @Override
    public void markOrGrowMarked(World w, int x, int y, int z, Random r) {
        if (checkArea(w, x, y, z) && w.rand.nextDouble() < EnumDyeTrees.VALID_FOLIAGE[w.getBlockMetadata(x, y, z)].growthChance)
            growTree(w, x, y, z, w.rand);
    }

    @Override
    public boolean onBlockActivated(World w, int x, int y, int z, EntityPlayer player, int p6, float a, float b, float c) {
        ItemStack stack = player.getCurrentEquippedItem();

        if (stack == null)
            return false;

        if (!(stack.getItem().itemID == PRColors.WHITE.getDye().getItem().itemID))
            return false;

        if (checkArea(w, x, y, z) && w.rand.nextDouble() < EnumDyeTrees.VALID_FOLIAGE[w.getBlockMetadata(x, y, z)].growthChance)
            growTree(w, x, y, z, w.rand);
        else
            w.spawnParticle("happyVillager", x + w.rand.nextDouble(), y + w.rand.nextDouble(), z + w.rand.nextDouble(), 0, 0, 0);

        if (!player.capabilities.isCreativeMode)
            stack.stackSize--;

        return true;
    }

    public static boolean checkArea(World w, int x, int y, int z) {
        int id = w.getBlockId(x, y-1, z);
        Material mat = Block.blocksList[id].blockMaterial;
        boolean validSoil = false;
        boolean validArea = true;

        if (mat == Material.grass || mat == Material.ground)
            validSoil=true;
        if (!validSoil)
            return false;

        BlockCoord bc = new BlockCoord(x,y,z);
        for (int i = 1; i < 6; i++) {
            BlockCoord bc2 = bc.copy().offset(i);
            Block b = blocksList[w.getBlockId(bc2.x, bc2.y, bc2.z)];
            if (!w.isAirBlock(bc2.x, bc2.y, bc2.z) && !b.canBeReplacedByLeaves(w, bc2.x, bc2.y, bc2.z) && b instanceof IPlantable) {
                validArea = false;
                break;
            }
        }

        return validSoil && validArea;
    }

    @Override
    public Icon getIcon(int par1, int par2) {
        return icon;
    }

    @Override
    public void getSubBlocks(int id, CreativeTabs tab, List list) {
        for (EnumDyeTrees t : EnumDyeTrees.VALID_FOLIAGE)
            list.add(t.getSappling());
    }

    @Override
    public int damageDropped(int meta) {
        return meta;
    }


    @Override
    public void registerIcons(IconRegister reg) {
        icon = reg.registerIcon("ProjectRed:ore/sapling");
    }

    @Override
    public int getRenderColor(int meta) {
        return PRColors.get(meta).rgb;
    }

    @Override
    public int colorMultiplier(IBlockAccess iba, int x, int y, int z) {
        int meta = iba.getBlockMetadata(x, y, z);
        return PRColors.get(meta).rgb;
    }
}
