package mrtjp.projectred.core.blockutil;

import codechicken.lib.vec.BlockCoord;
import cpw.mods.fml.common.registry.GameRegistry;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;
import mrtjp.projectred.core.BasicRenderUtils;
import mrtjp.projectred.core.BasicUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.material.Material;
import net.minecraft.enchantment.EnchantmentHelper;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;

import java.util.ArrayList;
import java.util.Random;

public class BlockMulti extends BlockContainer
{
    private Class[] tiles = new Class[16];

    public BlockMulti(int id, Material mat)
    {
        super(id, mat);
    }

    @Override
    public boolean isOpaqueCube()
    {
        return false;
    }

    @Override
    public boolean renderAsNormalBlock()
    {
        return false;
    }

    @Override
    public int damageDropped(int i)
    {
        return i;
    }

    @Override
    public int idDropped(int i, Random random, int j)
    {
        return 0;
    }

    @Override
    public void harvestBlock(World world, EntityPlayer player, int i, int j, int k, int l)
    {
    }

    @Override
    public int getRenderType()
    {
        return BasicRenderUtils.coreRenderHandlerID;
    }

    @Override
    public float getBlockHardness(World par1World, int par2, int par3, int par4)
    {
        return blockHardness;
    }

    @SideOnly(Side.CLIENT)
    public void randomDisplayTick(World w, int x, int y, int z, Random rand)
    {
        int md = w.getBlockMetadata(x, y, z);
        RenderMulti r = BasicRenderUtils.getRenderer(blockID, md);
        if (r != null)
            r.randomDisplayTick(w, x, y, z, rand);
    }

    public TileEntity getBlockEntity()
    {
        return null;
    }

    public void addTile(int meta, Class<? extends TileMulti> clazz, String name)
    {
        tiles[meta] = clazz;
        GameRegistry.registerTileEntity(clazz, name);
    }

    @Override
    public TileEntity createNewTileEntity(World world)
    {
        return null;
    }

    @Override
    public TileEntity createTileEntity(World world, int meta)
    {
        try
        {
            return (TileEntity) tiles[meta].newInstance();
        }
        catch (Exception e)
        {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    public boolean removeBlockByPlayer(World world, EntityPlayer player, int x, int y, int z)
    {
        if (BasicUtils.isClient(world))
            return true;

        int bid = world.getBlockId(x, y, z);
        int md = world.getBlockMetadata(x, y, z);
        Block bl = Block.blocksList[bid];
        if (bl == null)
            return false;
        if (bl.canHarvestBlock(player, md) && !player.capabilities.isCreativeMode)
        {
            ArrayList<ItemStack> il = getBlockDropped(world, x, y, z, md, EnchantmentHelper.getFortuneModifier(player));

            for (ItemStack it : il)
                BasicUtils.dropItem(world, x, y, z, it);
        }
        world.setBlock(x, y, z, 0);
        return true;
    }

    @Override
    public ArrayList getBlockDropped(World w, int x, int y, int z, int meta, int fortune)
    {
        ArrayList<ItemStack> list = new ArrayList<ItemStack>();

        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile == null)
            return list;
        tile.addHarvestContents(list);
        return list;
    }

    @Override
    public void onNeighborBlockChange(World w, int x, int y, int z, int l)
    {
        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile == null)
        {
            w.setBlock(x, y, z, 0);
            return;
        }
        tile.onBlockNeighborChange(l);
    }

    @Override
    public void onBlockPlacedBy(World w, int x, int y, int z, EntityLivingBase player, ItemStack stack)
    {
        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile == null)
            return;

        tile.onBlockPlaced(stack, 0, (EntityPlayer) player);
    }

    @Override
    public void breakBlock(World w, int x, int y, int z, int id, int md)
    {
        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile == null)
            return;

        tile.onBlockRemoval();
        super.breakBlock(w, x, y, z, id, md);
    }

    @Override
    public int isProvidingStrongPower(IBlockAccess w, int x, int y, int z, int l)
    {
        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile == null)
            return 0;

        return tile.isBlockStrongPoweringTo(l);
    }

    @Override
    public int isProvidingWeakPower(IBlockAccess w, int x, int y, int z, int l)
    {
        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile == null)
            return 0;

        return tile.isBlockWeakPoweringTo(l);
    }

    @Override
    public boolean onBlockActivated(World w, int x, int y, int z, EntityPlayer player, int side, float xp, float yp, float zp)
    {
        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile == null)
            return false;

        return tile.onBlockActivated(player, side);
    }

    @Override
    public void onEntityCollidedWithBlock(World w, int x, int y, int z, Entity entity)
    {
        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile == null)
            return;

        tile.onEntityCollidedWithBlock(entity);
    }

    @Override
    public AxisAlignedBB getCollisionBoundingBoxFromPool(World w, int x, int y, int z)
    {
        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile != null)
        {
            AxisAlignedBB bb = tile.getCollisionBoundingBox();
            if (bb != null)
                return bb;
        }
        return super.getCollisionBoundingBoxFromPool(w, x, y, z);
    }

    @Override
    public int getLightValue(IBlockAccess w, int x, int y, int z)
    {
        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile != null)
            return tile.getLightValue();

        return super.getLightValue(w, x, y, z);
    }

    @Override
    public boolean isFireSource(World w, int x, int y, int z, int meta, ForgeDirection side)
    {
        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile != null)
            return tile.isFireSource(side);

        return super.isFireSource(w, x, y, z, meta, side);
    }

    @Override
    public boolean isBlockSolidOnSide(World w, int x, int y, int z, ForgeDirection side)
    {
        TileMulti tile = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tile != null)
            return tile.isBlockSolidOnSide(side);

        return super.isBlockSolidOnSide(w, x, y, z, side);
    }
}