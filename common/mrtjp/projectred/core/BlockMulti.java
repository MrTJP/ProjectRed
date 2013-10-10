package mrtjp.projectred.core;

import java.util.ArrayList;
import java.util.Random;

import net.minecraft.block.Block;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.material.Material;
import net.minecraft.enchantment.EnchantmentHelper;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockMulti extends BlockContainer {

    private Class[] tileEntityMap = new Class[16];

    public BlockMulti(int i, Material m) {
        super(i, m);
    }

    @Override
    public boolean isOpaqueCube() {
        return false;
    }

    @Override
    public boolean renderAsNormalBlock() {
        return false;
    }

    @Override
    public int damageDropped(int i) {
        return i;
    }

    @Override
    public float getBlockHardness(World par1World, int par2, int par3, int par4) {
        return blockHardness;
    }

    @Override
    public ArrayList getBlockDropped(World w, int x, int y, int z, int meta, int fortune) {
        ArrayList ist = new ArrayList();

        TileMulti tl = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tl == null)
            return ist;
        tl.addHarvestContents(ist);
        return ist;
    }

    @Override
    public int idDropped(int i, Random random, int j) {
        return 0;
    }

    @Override
    public void harvestBlock(World world, EntityPlayer player, int i, int j, int k, int l) {
    }

    @Override
    public boolean removeBlockByPlayer(World world, EntityPlayer player, int x, int y, int z) {
        if (BasicUtils.isClient(world))
            return true;

        int bid = world.getBlockId(x, y, z);
        int md = world.getBlockMetadata(x, y, z);
        Block bl = Block.blocksList[bid];
        if (bl == null)
            return false;
        if ((bl.canHarvestBlock(player, md)) && (!player.capabilities.isCreativeMode)) {
            ArrayList<ItemStack> il = getBlockDropped(world, x, y, z, md, EnchantmentHelper.getFortuneModifier(player));

            for (ItemStack it : il)
                BasicUtils.dropItem(world, x, y, z, it);
        }
        world.setBlock(x, y, z, 0);
        return true;
    }

    @Override
    public void onNeighborBlockChange(World w, int x, int y, int z, int l) {
        TileMulti tl = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tl == null) {
            w.setBlock(x, y, z, 0);
            return;
        }
        tl.onBlockNeighborChange(l);
    }

    @Override
    public void onBlockPlacedBy(World w, int x, int y, int z, EntityLivingBase player, ItemStack stack) {
        TileMulti tl = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tl == null)
            return;
        
        tl.onBlockPlaced(stack, 0, (EntityPlayer) player);
    }

    @Override
    public void breakBlock(World w, int x, int y, int z, int id, int md) {
        TileMulti tl = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tl == null)
            return;
        
        tl.onBlockRemoval();
        super.breakBlock(w, x, y, z, id, md);
    }

    @Override
    public int isProvidingStrongPower(IBlockAccess w, int x, int y, int z, int l) {
        TileMulti tl = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tl == null)
            return 0;
        
        return tl.isBlockStrongPoweringTo(l);
    }

    @Override
    public int isProvidingWeakPower(IBlockAccess w, int x, int y, int z, int l) {
        TileMulti tl = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tl == null)
            return 0;
        
        return tl.isBlockWeakPoweringTo(l);
    }

    @Override
    public boolean onBlockActivated(World w, int x, int y, int z, EntityPlayer player, int side, float xp, float yp, float zp) {
        TileMulti tl = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tl == null)
            return false;
        
        return tl.onBlockActivated(player);
    }

    @Override
    public void onEntityCollidedWithBlock(World w, int x, int y, int z, Entity entity) {
        TileMulti tl = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tl == null)
            return;
        
        tl.onEntityCollidedWithBlock(entity);
    }

    public AxisAlignedBB getCollisionBoundingBoxFromPool(World w, int x, int y, int z) {
        TileMulti tl = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMulti.class);

        if (tl != null) {
            AxisAlignedBB bb = tl.getCollisionBoundingBox();
            if (bb != null)
                return bb;
        }
        return super.getCollisionBoundingBoxFromPool(w, x, y, z);
    }

    @Override
    public int getRenderType() {
        return BasicRenderUtils.coreRenderHandlerID;
    }

    @SideOnly(Side.CLIENT)
    public void randomDisplayTick(World w, int x, int y, int z, Random r) {
        int md = w.getBlockMetadata(x, y, z);
        RenderMulti rend = BasicRenderUtils.getRenderer(blockID, md);
        if (rend != null)
            rend.randomDisplayTick(w, x, y, z, r);
    }
    
    public TileEntity getBlockEntity() {
        return null;
    }

    public void addTileEntityMapping(int meta, Class cl) {
        tileEntityMap[meta] = cl;
    }

    public void setItemName(int meta, String name) {
        Item item = Item.itemsList[blockID];
        ((ItemBlockMulti) item).setMetaName(meta, "tile." + name);
    }

    @Override
    public TileEntity createNewTileEntity(World world) {
        return null;
    }

    @Override
    public TileEntity createTileEntity(World world, int meta) {
        try {
            return (TileEntity) tileEntityMap[meta].getDeclaredConstructor(new Class[0]).newInstance(new Object[0]);
        } catch (Exception e) {            
            return null;
        }
    }
}
