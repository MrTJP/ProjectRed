package mrtjp.projectred.exploration;

import java.util.Random;

import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;

/**
 * Special version of mossy cobblestone, that spreads to nearby blocks when
 * under proper conditions.
 * 
 * @author MrTJP
 * 
 */
public class BlockPhotosyntheticCobblestone extends Block {

    public BlockPhotosyntheticCobblestone(int i) {
        super(i, Material.rock);
        setHardness(2.0F);
        setResistance(10.0F);
        setStepSound(soundStoneFootstep);
        setUnlocalizedName("stoneMoss");
        setCreativeTab(CreativeTabs.tabBlock);
        setTextureName("cobblestone_mossy");
        setTickRandomly(true);
    }

    @Override
    public void updateTick(World w, int x, int y, int z, Random ran) {
        if (!w.isAirBlock(x, y + 1, z) || w.canBlockSeeTheSky(x, y + 1, z))
            return;

        for (int i = 0; i < 6; i++) {
            BlockCoord bc = new BlockCoord(x, y, z).offset(i);
            int id = w.getBlockId(bc.x, bc.y, bc.z);
            int meta = w.getBlockMetadata(bc.x, bc.y, bc.z);
            if (!w.isAirBlock(bc.x, bc.y + 1, bc.z) || w.canBlockSeeTheSky(bc.x, bc.y + 1, bc.z))
                continue;

            if (id == Block.cobblestone.blockID) {
                if (isBlockWet(w, bc))
                    if (ran.nextInt(3) == 0)
                        w.setBlock(bc.x, bc.y, bc.z, Block.cobblestoneMossy.blockID, 0, 3);
            } else if (id == Block.stoneBrick.blockID && meta == 2) {
                if (isBlockWet(w, bc))
                    if (ran.nextInt(3) == 0)
                        w.setBlock(bc.x, bc.y, bc.z, Block.stoneBrick.blockID, 1, 3);
            }
        }
    }

    public boolean isBlockWet(World w, BlockCoord b) {
        for (int i = 0; i < 6; i++) {
            BlockCoord bc = b.copy().offset(i);
            int id = w.getBlockId(bc.x, bc.y, bc.z);
            if (id == Block.waterMoving.blockID || id == Block.waterStill.blockID)
                return true;
        }
        return false;
    }
    
    @Override
    public void registerIcons(IconRegister reg) {
        super.registerIcons(reg);
        Block.cobblestoneMossy.registerIcons(reg);
    }
}
