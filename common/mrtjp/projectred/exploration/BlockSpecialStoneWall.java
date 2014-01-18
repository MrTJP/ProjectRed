package mrtjp.projectred.exploration;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;
import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import net.minecraft.block.Block;
import net.minecraft.block.BlockWall;
import net.minecraft.block.material.Material;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;

import java.util.List;

public class BlockSpecialStoneWall extends BlockWall
{
    public BlockSpecialStoneWall(int par1)
    {
        super(par1, Block.stone);
        this.setUnlocalizedName("projectred.exploration.stonewalls");
        setCreativeTab(ProjectRedExploration.tabExploration);
    }

    @SideOnly(Side.CLIENT)
    @Override
    public Icon getIcon(int side, int meta)
    {
        return BlockSpecialStone.EnumSpecialStone.VALID_STONE[meta].texture;
    }

    @SideOnly(Side.CLIENT)
    @Override
    public void getSubBlocks(int id, CreativeTabs tab, List list)
    {
        for (EnumSpecialStone s : EnumSpecialStone.VALID_STONE)
            list.add(new ItemStack(ProjectRedExploration.blockStoneWalls, 1, s.meta));
    }

    @Override
    /**
     * Return whether an adjacent block can connect to a wall.
     */
    public boolean canConnectWallTo(IBlockAccess w, int x, int y, int z)
    {
        int l = w.getBlockId(x, y, z);

        if (l != blockID && l != Block.fenceGate.blockID)
        {
            Block block = Block.blocksList[l];
            return block != null && block.blockMaterial.isOpaque() && block.renderAsNormalBlock() && block.blockMaterial != Material.pumpkin;
        }
        else
            return true;
    }

    @Override
    public boolean canPlaceTorchOnTop(World world, int x, int y, int z)
    {
        int id = world.getBlockId(x, y, z);
        return super.canPlaceTorchOnTop(world, x, y, z) || id == ProjectRedExploration.blockStoneWalls.blockID;
    }
}
