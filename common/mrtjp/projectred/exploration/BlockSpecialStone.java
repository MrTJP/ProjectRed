package mrtjp.projectred.exploration;

import java.util.ArrayList;

import mrtjp.projectred.ProjectRedExpansion;
import mrtjp.projectred.ProjectRedExploration;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.entity.Entity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.world.World;

public class BlockSpecialStone extends Block {

    public BlockSpecialStone(int par1) {
        super(par1, Material.rock);
        this.setUnlocalizedName("projectred.exploration.stone");
        setHardness(3.0F);
        setResistance(10.0F);
        setCreativeTab(ProjectRedExploration.tabExploration);
    }

    @Override
    public float getBlockHardness(World world, int x, int y, int z) {
        int meta = world.getBlockMetadata(x, y, z);
        return EnumSpecialStone.VALID_STONE[meta].hardness;
    }

    @Override
    public float getExplosionResistance(Entity exploder, World world, int x, int y, int z, double srcX, double srcY, double srcZ) {
        int meta = world.getBlockMetadata(x, y, z);
        return EnumSpecialStone.VALID_STONE[meta].explosionRes;
    }

    @Override
    public Icon getIcon(int side, int meta) {
        return EnumSpecialStone.VALID_STONE[meta].texture;
    }

    @Override
    public ArrayList<ItemStack> getBlockDropped(World world, int x, int y, int z, int meta, int fortune) {
        ArrayList<ItemStack> ret = new ArrayList<ItemStack>();
        EnumSpecialStone type = EnumSpecialStone.VALID_STONE[meta];
        ret.add(type.specailDrop == null ? type.getItemStack() : type.specailDrop.copy());
        return ret;
    }

    @Override
    public void registerIcons(IconRegister reg) {
        for (EnumSpecialStone s : EnumSpecialStone.VALID_STONE) {
            s.loadTextures(reg);
        }
    }

    public enum EnumSpecialStone {
        MARBLE("Marble", "stonemarble", 1, 6),
        MARBLEBRICK("Marble Brick", "brickmarble", 1, 6), 
        BASALTCOBBLE("Basalt Cobblestone", "cobblebasalt", 2.5f, 16),
        BASALT("Basalt", "stonebasalt", 2.5f, 8, BASALTCOBBLE.getItemStack()),
        BASALTBRICK("Basalt Brick", "brickbasalt", 2.5f, 8),
        ;

        public final String name;
        public final String unlocal;
        public final ItemStack specailDrop;
        public final float hardness;
        public final float explosionRes;
        public final int meta = ordinal();
        public Icon texture;
        public static EnumSpecialStone[] VALID_STONE = values();

        private EnumSpecialStone(String name, String unlocal, float hardness, float explosionRes) {
            this(name, unlocal, hardness, explosionRes, null);
        }

        private EnumSpecialStone(String name, String unlocal, float hardness, float explosionRes, ItemStack drop) {
            this.name = name;
            this.unlocal = unlocal;
            this.hardness = hardness;
            this.explosionRes = explosionRes;
            this.specailDrop = drop;
        }

        public void loadTextures(IconRegister reg) {
            texture = reg.registerIcon("projectred:ore/" + unlocal);
        }

        public ItemStack getItemStack() {
            return getItemStack(1);
        }
        
        public ItemStack getItemStack(int i) {
            return new ItemStack(ProjectRedExploration.blockStones, 1, meta);
        }
    }
}
