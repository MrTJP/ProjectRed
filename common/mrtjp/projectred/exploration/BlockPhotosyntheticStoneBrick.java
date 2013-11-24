package mrtjp.projectred.exploration;

import java.util.Random;

import net.minecraft.block.Block;
import net.minecraft.block.BlockStoneBrick;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.util.Icon;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockPhotosyntheticStoneBrick extends BlockStoneBrick {

    public static final String[] STONE_BRICK_TYPES = new String[] { "default", "mossy", "cracked", "chiseled" };
    public static final String[] names = new String[] { null, "mossy", "cracked", "carved" };
    @SideOnly(Side.CLIENT)
    private Icon[] icons;

    public BlockPhotosyntheticStoneBrick(int par1) {
        super(par1);
        setHardness(1.5F);
        setResistance(10.0F);
        setStepSound(soundStoneFootstep);
        setUnlocalizedName("stonebricksmooth");
        setTextureName("stonebrick");
        setTickRandomly(true);
    }

    @Override
    public void updateTick(World w, int x, int y, int z, Random ran) {
        switch (w.getBlockMetadata(x, y, z)) {
        case 0:
            crackFromHeat(w, x, y, z, ran);
            return;
        case 1:
            spreadMossToNearby(w, x, y, z, ran);
            return;
        }
    }

    public void crackFromHeat(World w, int x, int y, int z, Random ran) {
        BlockCoord bc = new BlockCoord(x, y, z);
        if (isBlockWet(w, bc) && isBlockHot(w, bc))
            if (ran.nextInt(3) == 0)
                w.setBlock(x, y, z, Block.stoneBrick.blockID, 2, 3);
    }

    public void spreadMossToNearby(World w, int x, int y, int z, Random ran) {
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
            } else if (id == Block.stoneBrick.blockID && meta == 2)
                if (isBlockWet(w, bc))
                    if (ran.nextInt(3) == 0)
                        w.setBlock(bc.x, bc.y, bc.z, Block.stoneBrick.blockID, 1, 3);
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

    public boolean isBlockHot(World w, BlockCoord b) {
        for (int i = 0; i < 6; i++) {
            BlockCoord bc = b.copy().offset(i);
            int id = w.getBlockId(bc.x, bc.y, bc.z);
            if (id == Block.lavaMoving.blockID || id == Block.lavaStill.blockID)
                return true;
        }
        return false;
    }

    @Override
    public void registerIcons(IconRegister reg) {
        super.registerIcons(reg);
        icons = new Icon[names.length];
        Block.stoneBrick.registerIcons(reg);
        for (int i = 0; i < this.icons.length; ++i) {
            String s = this.getTextureName();
            if (names[i] != null)
                s = s + "_" + names[i];
            this.icons[i] = reg.registerIcon(s);
        }
    }

    @Override
    @SideOnly(Side.CLIENT)
    public Icon getIcon(int par1, int par2) {
        if (icons == null)
            icons = new Icon[names.length];
        if (par2 < 0 || par2 >= names.length)
            par2 = 0;
        return this.icons[par2];
    }

}
