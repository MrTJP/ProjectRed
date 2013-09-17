package mrtjp.projectred.illumination;

import java.util.ArrayList;
import java.util.List;

import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.BasicUtils;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.EnumCreatureType;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockLamp extends Block {

    public static Icon[] onIcons = new Icon[16];
    public static Icon[] offIcons = new Icon[16];

    public BlockLamp(int id) {
        super(id, new Material(Material.circuits.materialMapColor));
        setBlockBounds(0.0F, 0.0F, 0.0F, 1.0F, 1.0F, 1.0F);
        setHardness(0.5F);
        setUnlocalizedName("projectred.illumination.lamp");
        setCreativeTab(ProjectRedIllumination.tabLighting);
    }

    @Override
    public void onNeighborBlockChange(World world, int x, int y, int z, int id) {
        TileLamp tile = (TileLamp) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileLamp.class);
        if (tile != null) {
            tile.onNeighborBlockChange();
        }
    }

    @Override
    public boolean renderAsNormalBlock() {
        return true;
    }

    @Override
    public int getRenderType() {
        return IlluminationClientProxy.lampRenderID;
    }

    @Override
    public boolean isOpaqueCube() {
        return true;
    }

    @Override
    public boolean isBlockNormalCube(World world, int x, int y, int z) {
        return true;
    }

    @Override
    public void getSubBlocks(int id, CreativeTabs tab, List list) {
        for (int i = 0; i < 32; i++) {
            list.add(new ItemStack(id, 1, i));
        }
    }

    @Override
    public int getLightValue(IBlockAccess world, int x, int y, int z) {
        TileLamp tile = (TileLamp) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileLamp.class);
        if (tile != null) {
            return tile.getLightValue();
        }
        return 0;
    }

    @Override
    public boolean canCreatureSpawn(EnumCreatureType type, World world, int x, int y, int z) {
        return false;
    }

    @Override
    public boolean canConnectRedstone(IBlockAccess world, int x, int y, int z, int side) {
        return true;
    }

    @Override
    public boolean canProvidePower() {
        return true;
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void registerIcons(IconRegister reg) {
        for (int i = 0; i < 16; i++) {
            onIcons[i] = reg.registerIcon("projectred:lights/lampon/" + i);
            offIcons[i] = reg.registerIcon("projectred:lights/lampoff/" + i);
        }
    }

    @Override
    public ItemStack getPickBlock(MovingObjectPosition target, World world, int x, int y, int z) {
        TileLamp tile = (TileLamp) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileLamp.class);
        if (tile != null) {
            return tile.getDroppedBlock();
        }
        return null;
    }

    @Override
    public boolean removeBlockByPlayer(World world, EntityPlayer player, int x, int y, int z) {
        TileLamp tile = (TileLamp) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileLamp.class);
        if (tile != null && !player.capabilities.isCreativeMode) {
            BasicUtils.dropItem(world, x, y, z, tile.getDroppedBlock());
        }
        return super.removeBlockByPlayer(world, player, x, y, z);
    }

    public ArrayList<ItemStack> getBlockDropped(World world, int x, int y, int z, int metadata, int fortune) {
        return new ArrayList<ItemStack>(); // Handled on removeBlockByPlayer
    }

    @Override
    public Icon getBlockTexture(IBlockAccess world, int x, int y, int z, int side) {
        TileLamp tile = (TileLamp) BasicUtils.getTileEntity(world, new BlockCoord(x, y, z), TileLamp.class);
        if (tile != null) {
            if (tile.getLightValue() == 15)
                return onIcons[tile.color];
            else
                return offIcons[tile.color];
        }
        return super.getBlockTexture(world, x, y, z, side);
    }

    @Override
    public Icon getIcon(int side, int meta) {
        if (meta > 15) {
            return onIcons[meta - 16];
        } else {
            return offIcons[meta];
        }
    }

    @Override
    public TileEntity createTileEntity(World world, int meta) {
        TileLamp t = new TileLamp();
        t.prepairPlacement(meta>15, meta>15?meta-15:meta);
        return t;
    }

    @Override
    public boolean hasTileEntity(int meta) {
        return true;
    }
}
