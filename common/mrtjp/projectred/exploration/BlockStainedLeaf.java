package mrtjp.projectred.exploration;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import net.minecraft.block.Block;
import net.minecraft.block.BlockLeaves;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import cpw.mods.fml.common.FMLCommonHandler;
import cpw.mods.fml.relauncher.Side;

public class BlockStainedLeaf extends BlockLeaves {

    private Icon[] icon = new Icon[2];

    public BlockStainedLeaf(int ID) {
        super(ID);
        setUnlocalizedName("projectred.exploration.dyeleaf");
        setCreativeTab(ProjectRedExploration.tabExploration);
        setLightOpacity(1);
        setStepSound(Block.soundGrassFootstep);
        setHardness(0.2F);
        
        if (FMLCommonHandler.instance().getEffectiveSide() == Side.CLIENT)
            setGraphicsLevel(Minecraft.getMinecraft().gameSettings.fancyGraphics);
    }

    @Override
    public int getRenderColor(int meta) {
        return PRColors.get(meta).rgb;
    }

    @Override
    public int colorMultiplier(IBlockAccess w, int x, int y, int z) {
        return PRColors.get(w.getBlockMetadata(x, y, z)).rgb;
    }

    @Override
    public int idDropped(int id, Random r, int f) {
        return EnumDyeTrees.VALID_FOILAGE[id].getSappling().itemID;
    }

    @Override
    public int damageDropped(int meta) {
        return meta;
    }

    @Override
    public void dropBlockAsItemWithChance(World w, int x, int y, int z, int meta, float chance, int fortune) {
        if (!w.isRemote) {
            if (w.rand.nextDouble() < EnumDyeTrees.VALID_FOILAGE[meta].saplingChance*(1 + fortune))
                this.dropBlockAsItem_do(w, x, y, z, EnumDyeTrees.VALID_FOILAGE[meta].getSappling());
            
            if (w.rand.nextDouble() < EnumDyeTrees.VALID_FOILAGE[meta].appleChance*(1 + fortune * 5))
                this.dropBlockAsItem_do(w, x, y, z, new ItemStack(Item.appleRed, 1, 0));
        }
    }

    @Override
    public Icon getIcon(int par1, int par2) {
        return icon[graphicsLevel ? 0 : 1];
    }

    @Override
    public void registerIcons(IconRegister reg) {
        icon[0] = reg.registerIcon("ProjectRed:ore/leaves");
        icon[1] = reg.registerIcon("ProjectRed:ore/leaves1");
    }

    @Override
    public ArrayList<ItemStack> onSheared(ItemStack item, World world, int x, int y, int z, int fortune) {
        ArrayList<ItemStack> ret = new ArrayList<ItemStack>();
        ret.add(new ItemStack(this, 1, world.getBlockMetadata(x, y, z)));
        return ret;
    }

    @Override
    protected ItemStack createStackedBlock(int par1) {
        return new ItemStack(blockID, 1, par1);
    }

    @Override
    public void randomDisplayTick(World world, int x, int y, int z, Random rand) {
        //TODO particles
    }

    @Override
    public void beginLeavesDecay(World world, int x, int y, int z) {
        world.setBlockMetadataWithNotify(x, y, z, world.getBlockMetadata(x, y, z), 4);
    }

    @Override
    public int getFlammability(IBlockAccess world, int x, int y, int z, int metadata, ForgeDirection face) {
        return 30;
    }

    @Override
    public int getFireSpreadSpeed(World world, int x, int y, int z, int metadata, ForgeDirection face) {
        return 60;
    }
    
    @Override
    public void getSubBlocks(int id, CreativeTabs tab, List list) {
        for (EnumDyeTrees t : EnumDyeTrees.VALID_FOILAGE)
            list.add(t.getLeaf());
    }

    enum EnumDyeTrees {
        WHITE(      0.045F,     0.005F,     0.08F),
        ORANGE(     0.05F,      0.005F,     0.09F),
        MAGENTA(    0.05F,      0.005F,     0.1F),
        LIGHT_BLUE( 0.045F,     0.005F,     0.08F),
        YELLOW(     0.05F,      0.005F,     0.1F),
        LIME(       0.045F,     0.005F,     0.09F),
        PINK(       0.045F,     0.005F,     0.09F),
        GREY(       0.045F,     0.005F,     0.08F),
        LIGHT_GREY( 0.045F,     0.005F,     0.08F),
        CYAN(       0.04F,      0.005F,     0.08F),
        PURPLE(     0.045F,     0.005F,     0.09F),
        BLUE(       0.04F,      0.005F,     0.075F),
        BROWN(      0.04F,      0.005F,     0.075F),
        GREEN(      0.045F,     0.005F,     0.08F),
        RED(        0.05F,      0.005F,     0.1F),
        BLACK(      0.04F,      0.005F,     0.075F);
        
        public static final EnumDyeTrees[] VALID_FOILAGE = values();
        
        public final float saplingChance;
        public final float appleChance;
        public final float growthChance;
        public final int meta = ordinal();
        
        private EnumDyeTrees(float saplingChance, float appleChance, float growthChance) {
            this.saplingChance = saplingChance;
            this.appleChance = appleChance;
            this.growthChance = growthChance;
        }
        
        public ItemStack getSappling() {
            return new ItemStack(ProjectRedExploration.blockStainedSapling, 1, this.meta);
        }
        
        public ItemStack getLeaf() {
            return new ItemStack(ProjectRedExploration.blockStainedLeaf, 1, this.meta);
        }
    }
}