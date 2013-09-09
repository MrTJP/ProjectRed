package mrtjp.projectred.core;

import java.util.ArrayList;
import java.util.List;

import mrtjp.projectred.ProjectRedCore;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockBasics extends BlockContainer {

    public BlockBasics(int id) {
        super(id, new Material(Material.iron.materialMapColor));
        setCreativeTab(ProjectRedCore.tabCore);
        this.setUnlocalizedName("projectred.core.appliance");
        setHardness(0.75f);
    }

    @Override
    public TileEntity createNewTileEntity(World world) {
        return null;
    }

    @Override
    public TileEntity createTileEntity(World world, int metadata) {
        try {
            return EnumBasics.get(metadata).clazz.newInstance();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public boolean hasTileEntity(int meta) {
        return true;
    }

    @Override
    public void breakBlock(World world, int x, int y, int z, int par5, int par6) {
        TileBasicsBase tile = (TileBasicsBase) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileBasicsBase.class);
        if (tile != null)
            tile.onBlockBreak();
        super.breakBlock(world, x, y, z, par5, par6);
        world.removeBlockTileEntity(x, y, z);
    }

    @Override
    public void onBlockClicked(World world, int x, int y, int z, EntityPlayer player) {
        TileBasicsBase tile = (TileBasicsBase) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileBasicsBase.class);
        if (tile != null)
            tile.onBlockClicked(player);
    }

    @Override
    public boolean onBlockActivated(World world, int x, int y, int z, EntityPlayer player, int par6, float par7, float par8, float par9) {
        TileBasicsBase tile = (TileBasicsBase) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileBasicsBase.class);
        if (tile != null)
            return tile.onBlockActivated(player);
        return false;
    }

    @Override
    public void onBlockPlacedBy(World world, int x, int y, int z, EntityLivingBase entity, ItemStack itemstack) {
        TileBasicsBase tile = (TileBasicsBase) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileBasicsBase.class);
        if (tile != null)
            tile.onBlockPlacedBy(entity, itemstack);
    }

    @Override
    public final ArrayList<ItemStack> getBlockDropped(World world, int x, int y, int z, int metadata, int fortune) {
        return new ArrayList<ItemStack>();
    }

    @Override
    public void getSubBlocks(int id, CreativeTabs tab, List list) {
        for (EnumBasics machine : EnumBasics.VALID_MACHINES)
            list.add(new ItemStack(id, 1, machine.meta));
    }

    @Override
    public void registerIcons(IconRegister reg) {
        for (EnumBasics m : EnumBasics.VALID_MACHINES) {
            m.icons = new Icon[m.iconPath.length];
            for (int i = 0; i < m.iconPath.length; i++)
                m.icons[i] = reg.registerIcon("projectred:machines/" + m.iconPath[i]);
        }
    }

    @SideOnly(Side.CLIENT)
    @Override
    public Icon getBlockTexture(IBlockAccess access, int x, int y, int z, int side) {
        TileBasicsBase tile = (TileBasicsBase) BasicUtils.getTileEntity(access, new BlockCoord(x, y, z), TileBasicsBase.class);
        if (tile != null)
            return tile.getType().icons[tile.getIconForSide(side)];
        return null;
    }

    @SideOnly(Side.CLIENT)
    @Override
    public Icon getIcon(int side, int meta) {
        return EnumBasics.get(meta).icons[EnumBasics.get(meta).textProvider.getIconIndex(side)];
    }

    @Override
    public int getLightValue(IBlockAccess world, int x, int y, int z) {
        TileBasicsBase tile = (TileBasicsBase) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileBasicsBase.class);
        if (tile != null)
            return tile.getLightLevel();
        return 0;
    }

    public enum EnumBasics {
        ALLOYSMELTER("Alloy Smelter", "machinealloy", TileAlloySmelter.class, new IIconIndexer() {
            @Override
            public int getIconIndex(int side) {
                return side == 0 || side == 1 ? 0 : side == 3 ? 2 : 1;
            }
        }, "smeltertop", "smelterside", "smelterfront", "smelterfronton");

        public static final EnumBasics[] VALID_MACHINES = { ALLOYSMELTER };

        public String fullname;
        public String unlocalname;
        public Class<? extends TileBasicsBase> clazz;
        public int meta = this.ordinal();

        public String[] iconPath = new String[6];
        public Icon[] icons;

        public IIconIndexer textProvider;

        private EnumBasics(String name, String unlocal, Class<? extends TileBasicsBase> tile, IIconIndexer p, String... sides) {
            fullname = name;
            unlocalname = unlocal;
            clazz = tile;
            iconPath = sides;
            textProvider = p;
        }

        public static EnumBasics get(int ordinal) {
            if (ordinal > VALID_MACHINES.length - 1)
                return null;
            return VALID_MACHINES[ordinal];
        }

        public ItemStack getItemStack() {
            return getItemStack(1);
        }

        public ItemStack getItemStack(int i) {
            return new ItemStack(ProjectRedCore.blockMachines, i, meta);
        }

        interface IIconIndexer {
            public int getIconIndex(int side);
        }
    }
}
