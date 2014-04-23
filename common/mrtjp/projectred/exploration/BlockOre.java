package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.util.MathHelper;
import net.minecraft.world.World;

import java.util.ArrayList;
import java.util.List;

public class BlockOre extends Block
{
    public BlockOre(int par1)
    {
        super(par1, Material.rock);
        setUnlocalizedName("projectred.exploration.ore");
        setHardness(3.0F);
        setResistance(5.0F);
        setCreativeTab(ProjectRedExploration.tabExploration());
    }

    @Override
    public ArrayList<ItemStack> getBlockDropped(World world, int x, int y, int z, int meta, int fortune)
    {
        ArrayList<ItemStack> ret = new ArrayList<ItemStack>();

        EnumOre type = EnumOre.VALID_ORES[meta];
        int min = type.minDrop;
        int max = type.maxDrop;

        if (min == max)
        {
            if (type.drop != null)
                ret.add(type.getDropStack(max));
            else
                ret.add(type.getItemStack(max));
            return ret;
        }

        int count = world.rand.nextInt(fortune + max);
        if (count > max)
            count = max;
        if (count < min)
            count = min;
        ret.add(type.getDropStack(count));
        return ret;
    }

    @Override
    public int getExpDrop(World world, int data, int enchantmentLevel)
    {
        EnumOre type = EnumOre.VALID_ORES[data];
        return MathHelper.getRandomIntegerInRange(world.rand, type.minXP, type.maxXP);
    }

    @Override
    public Icon getIcon(int side, int meta)
    {
        if (meta >= 0 && meta < EnumOre.VALID_ORES.length)
            return EnumOre.VALID_ORES[meta].texture;
        else return null;
    }

    @Override
    public void registerIcons(IconRegister reg)
    {
        for (EnumOre o : EnumOre.VALID_ORES)
            o.loadTextures(reg);
    }

    @Override
    public int damageDropped(int meta)
    {
        return meta;
    }

    @Override
    public void getSubBlocks(int id, CreativeTabs tab, List list)
    {
        for (EnumOre e : EnumOre.VALID_ORES)
            list.add(e.getItemStack(1));
    }

    public enum EnumOre
    {
        ORERUBY("Ruby Ore", "oreruby", 2, EnumPart.RUBY.getItemStack(), 1, 4, 1, 8),
        ORESAPPHIRE("Sapphire Ore", "oresapphire", 2, EnumPart.SAPPHIRE.getItemStack(), 1, 4, 1, 8),
        OREPERIDOT("Peridot Ore", "oreperidot", 2, EnumPart.PERIDOT.getItemStack(), 1, 4, 1, 8), ;

        public final String name;
        public final String unlocal;
        public final int harvestLevel;
        public final ItemStack drop;
        public final int minDrop;
        public final int maxDrop;
        public final int minXP;
        public final int maxXP;

        public final int meta = this.ordinal();
        public Icon texture;

        public static final EnumOre[] VALID_ORES = values();

        private EnumOre(String name, String unlocal, int harvestLevel, ItemStack drop, int min, int max)
        {
            this(name, unlocal, harvestLevel, drop, min, max, 0, 0);
        }

        private EnumOre(String name, String unlocal, int harvestLevel, ItemStack drop, int min, int max, int minXP, int maxXP)
        {
            this.name = name;
            this.unlocal = unlocal;
            this.harvestLevel = harvestLevel;
            this.drop = drop;
            this.minDrop = min;
            this.maxDrop = max;
            this.minXP = minXP;
            this.maxXP = maxXP;
        }

        public void loadTextures(IconRegister reg)
        {
            texture = reg.registerIcon("projectred:ore/" + unlocal);
        }

        public ItemStack getItemStack(int i)
        {
            return new ItemStack(ProjectRedExploration.blockOres(), i, meta);
        }

        public ItemStack getDropStack(int i)
        {
            return new ItemStack(drop.getItem(), i, drop.getItemDamage());
        }
    }
}
