package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.exploration.BlockStainedLeaf.EnumDyeTrees;
import net.minecraft.block.Block;
import net.minecraft.block.BlockSapling;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;

import java.util.List;
import java.util.Random;

public class BlockStainedSapling extends BlockSapling
{
    private Icon icon;
    private static Random r = new Random();

    public BlockStainedSapling(int par1)
    {
        super(par1);
        setStepSound(Block.soundGrassFootstep);
        setUnlocalizedName("projectred.exploration.dyesapling");
        setCreativeTab(ProjectRedExploration.tabExploration());
    }

    @Override
    public void growTree(World w, int x, int y, int z, Random r)
    {
        growTreeAt(w, x, y, z, height());
    }

    private int height() {
        return 5+r.nextInt(3);
    }

    @Override
    public void markOrGrowMarked(World w, int x, int y, int z, Random r)
    {
        growTree(w, x, y, z, r);
    }

    @Override
    public boolean onBlockActivated(World w, int x, int y, int z, EntityPlayer player, int p6, float a, float b, float c)
    {
        ItemStack stack = player.getCurrentEquippedItem();

        if (stack == null)
            return false;

        if (!(stack.getItem().itemID == PRColors.WHITE.getDye().getItem().itemID))
            return false;

        int color = w.getBlockMetadata(x, y, z);
        if (GeneratorColorTree.canGrowAt(w, x, y, z))
            growTreeAt(w, x, y, z, player.isSneaking() ? 7 : height());
        else
            w.spawnParticle("happyVillager", x + r.nextDouble(), y + r.nextDouble(), z + r.nextDouble(), 0, 0, 0);

        if (!player.capabilities.isCreativeMode)
            stack.stackSize--;

        return true;
    }

    private void growTreeAt(World w, int x, int y, int z, int h)
    {
        if (w.isRemote)
            return;
        int meta = w.getBlockMetadata(x, y, z);

        new GeneratorColorTree(ProjectRedExploration.blockStainedLeaf().blockID).generateTreeAnyType(w, x, y, z, PRColors.get(meta));
    }

    @Override
    public Icon getIcon(int par1, int par2)
    {
        return icon;
    }

    @Override
    public void getSubBlocks(int id, CreativeTabs tab, List list)
    {
        for (EnumDyeTrees t : EnumDyeTrees.VALID_FOLIAGE)
            list.add(t.getSappling());
    }

    @Override
    public int damageDropped(int meta)
    {
        return meta;
    }

    @Override
    public void registerIcons(IconRegister reg)
    {
        icon = reg.registerIcon("ProjectRed:ore/sapling");
    }

    @Override
    public int getRenderColor(int meta)
    {
        return PRColors.get(meta).rgb;
    }

    @Override
    public int colorMultiplier(IBlockAccess iba, int x, int y, int z)
    {
        int meta = iba.getBlockMetadata(x, y, z);
        return PRColors.get(meta).rgb;
    }
}
