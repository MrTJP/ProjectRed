package mrtjp.projectred.illumination;

import java.util.Random;

import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.fx.CoreParticle;
import mrtjp.projectred.core.fx.ParticleLogicApproachPoint;
import mrtjp.projectred.core.fx.ParticleLogicIconShift;
import mrtjp.projectred.core.fx.ParticleLogicOrbitPoint;
import mrtjp.projectred.core.fx.ParticleLogicScale;
import mrtjp.projectred.core.fx.ParticleManagement;
import net.minecraft.block.Block;
import net.minecraft.block.material.MapColor;
import net.minecraft.block.material.Material;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.Vector3;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BlockAirousLight extends Block
{
    private static class MaterialAirous extends Material
    {
        public static MaterialAirous materialAirous = new MaterialAirous();

        public MaterialAirous()
        {
            super(MapColor.airColor);
            setNoPushMobility();
            setRequiresTool();
        }

        @Override
        public boolean isSolid()
        {
            return false;
        }

        @Override
        public boolean isReplaceable()
        {
            return false;
        }

        @Override
        public boolean getCanBlockGrass()
        {
            return false;
        }

        @Override
        public boolean blocksMovement()
        {
            return true;
        }

        @Override
        protected Material setRequiresTool()
        {
            return this;
        }
    }

    public BlockAirousLight(int par1)
    {
        super(par1, MaterialAirous.materialAirous);
    }

    @Override
    public boolean isAirBlock(World world, int x, int y, int z)
    {
        return true;
    }

    @Override
    public int getLightValue(IBlockAccess world, int x, int y, int z)
    {
        return 14;
    }

    @Override
    public boolean isBlockReplaceable(World world, int x, int y, int z)
    {
        return true;
    }

    @Override
    public boolean isBlockSolidOnSide(World world, int x, int y, int z, ForgeDirection side)
    {
        return false;
    }

    @Override
    public int getRenderType()
    {
        return -1;
    }

    @Override
    public boolean renderAsNormalBlock()
    {
        return false;
    }

    @Override
    public boolean isOpaqueCube()
    {
        return false;
    }

    @SideOnly(Side.CLIENT)
    @Override
    public void randomDisplayTick(World world, int x, int y, int z, Random rand)
    {
        if (rand.nextInt(10) > 0)
            return;

        int color = world.getBlockMetadata(x, y, z);
        int dist = 3;

        int dx = x + rand.nextInt(dist) - rand.nextInt(dist);
        int dy = y + rand.nextInt(dist) - rand.nextInt(dist);
        int dz = z + rand.nextInt(dist) - rand.nextInt(dist);

        int ex = dx + rand.nextInt(dist) - rand.nextInt(dist);
        int ey = dy + rand.nextInt(dist) - rand.nextInt(dist);
        int ez = dz + rand.nextInt(dist) - rand.nextInt(dist);

        CoreParticle c = ParticleManagement.instance.spawn(world, "ember", dx, dy, dz);
        if (c != null)
        {
            ParticleLogicOrbitPoint orbit = new ParticleLogicOrbitPoint(new Vector3(ex, ey, ez));
            orbit.setOrbitSpeed(0.5f * rand.nextDouble()).setTargetDistance(0.3D);
            orbit.setShrinkingOrbit(0.01, 0.01).setPriority(2);

            ParticleLogicScale scale = new ParticleLogicScale();
            scale.setRate(-0.001F, -0.0001F * rand.nextFloat());
            scale.setTerminate(true);

            ParticleLogicIconShift iconshift = ParticleLogicIconShift.fluttering();

            ParticleLogicApproachPoint approach = new ParticleLogicApproachPoint(new Vector3(ex, ey, ez), 0.03f, 0.5f);
            approach.setFinal(true);

            c.setIgnoreMaxAge(true);
            c.setScale(0.05f + 0.02f * rand.nextFloat());
            c.setPRColor(PRColors.get(color));

            c.addLogic(orbit);
            c.addLogic(scale);
            c.addLogic(iconshift);
            c.addLogic(approach);
        }
    }

    @Override
    public TileEntity createTileEntity(World world, int metadata)
    {
        return new TileAirousLight();
    }

    @Override
    public boolean hasTileEntity(int metadata)
    {
        return true;
    }

    @Override
    public AxisAlignedBB getCollisionBoundingBoxFromPool(World par1World, int par2, int par3, int par4)
    {
        return null;
    }

    @Override
    public AxisAlignedBB getSelectedBoundingBoxFromPool(World par1World, int par2, int par3, int par4)
    {
        return AxisAlignedBB.getBoundingBox(0.0D, 0.0D, 0.0D, 0.0D, 0.0D, 0.0D);
    }

    @Override
    public boolean getBlocksMovement(IBlockAccess par1iBlockAccess, int par2, int par3, int par4)
    {
        return true;
    }
}
