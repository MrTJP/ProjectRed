package mrtjp.projectred.expansion;

import net.minecraft.block.material.Material;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import mrtjp.projectred.ProjectRedExpansion;
import mrtjp.projectred.core.blockutil.BlockMulti;

public class BlockMachine extends BlockMulti
{
    public BlockMachine(int id) {
        super(id, Material.rock);
        setHardness(2);
        setCreativeTab(ProjectRedExpansion.tabExpansion);
    }

    @Override
    public boolean isOpaqueCube() {
        return true;
    }

    @Override
    public boolean renderAsNormalBlock() {
        return true;
    }
    
    @Override
    public boolean isBlockSolidOnSide(World world, int x, int y, int z, ForgeDirection side) {
        return true;
    }
}
