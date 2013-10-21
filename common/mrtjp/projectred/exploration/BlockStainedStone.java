package mrtjp.projectred.exploration;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.PRColors;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;

public class BlockStainedStone extends Block {
    private final Icon[] icon = new Icon[1];

    public BlockStainedStone(int ID) {
        super(ID, Material.rock);
        setUnlocalizedName("projectred.exploration.dyestone");
        setCreativeTab(ProjectRedExploration.tabExploration);
        setStepSound(Block.soundStoneFootstep);
        setHardness(2.0F);
        setResistance(10.0F);
    }

    @Override
    public int colorMultiplier(IBlockAccess w, int x, int y, int z) {
        return PRColors.get(w.getBlockMetadata(x, y, z)).rgb;
    }

    @Override
    protected ItemStack createStackedBlock(int par1) {
        return new ItemStack(blockID, 1, par1);
    }

    @Override
    public int damageDropped(int meta) {
        return meta;
    }

    @Override
    public ArrayList<ItemStack> getBlockDropped(World world, int x, int y, int z, int meta, int fortune) {
        ArrayList<ItemStack> ret = new ArrayList<ItemStack>();
        EnumStainedStone type = EnumStainedStone.VALID_TYPES[meta];
        ret.add(type.specailDrop == null ? type.getItemStack() : type.specailDrop.copy());
        return ret;
    }

    @Override
    public Icon getIcon(int par1, int par2) {
        return icon[0];
    }

    @Override
    public int getRenderColor(int meta) {
        return PRColors.get(meta).rgb;
    }

    @Override
    public void getSubBlocks(int id, CreativeTabs tab, List list) {
        for (EnumStainedStone t : EnumStainedStone.VALID_TYPES)
            list.add(t.getStone());
    }

    @Override
    public int idDropped(int id, Random r, int f) {
        return id;
    }

    @Override
    public void randomDisplayTick(World world, int x, int y, int z, Random rand) {
        // TODO particles
    }

    @Override
    public void registerIcons(IconRegister reg) {
        icon[0] = reg.registerIcon("ProjectRed:ore/dyestone");
    }

    enum EnumStainedStone {
        WHITE("White Stained Brick", null),
        ORANGE("Orange Stained Brick", null),
        MAGENTA("Magenta Stained Brick", null),
        LIGHT_BLUE("Light Blue Stained Brick", null),
        YELLOW("Yellow Stained Brick", null),
        LIME("Lime Stained Brick", null),
        PINK("Pink Stained Brick", null),
        GREY("Grey Stained Brick", null),
        LIGHT_GREY("Light Grey Stained Brick", null),
        CYAN("Cyan Stained Brick", null),
        PURPLE("Purple Stained Brick", null),
        BLUE("Blue Stained Brick", null),
        BROWN("Brown Stained Brick", null),
        GREEN("Green Stained Brick", null),
        RED("Red Stained Brick", null),
        BLACK("Black Stained Brick", null), ;

        public static final EnumStainedStone[] VALID_TYPES = values();
        public final String                    name;
        public final ItemStack                 specailDrop;
        public final int                       meta        = ordinal();

        private EnumStainedStone(String name, ItemStack specailDrop) {
            this.name = name;
            this.specailDrop = specailDrop;
        }

        public ItemStack getItemStack() {
            return getItemStack(1);
        }

        public ItemStack getItemStack(int i) {
            return new ItemStack(ProjectRedExploration.blockStainedStone, i, meta);
        }

        public ItemStack getStone() {
            return new ItemStack(ProjectRedExploration.blockStainedStone, 1, meta);
        }
    }
}
