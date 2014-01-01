package mrtjp.projectred.compatibility.tconstruct;

import net.minecraft.block.material.Material;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.item.EntityItem;
import net.minecraft.util.DamageSource;
import net.minecraft.util.Icon;
import net.minecraft.world.World;
import net.minecraftforge.fluids.BlockFluidFinite;
import net.minecraftforge.fluids.Fluid;
import tconstruct.library.TConstructRegistry;

public class LiquidFiniteSubstance extends BlockFluidFinite
{
    String texture;
    public Icon stillIcon;
    public Icon flowIcon;

    public LiquidFiniteSubstance(int id, Fluid fluid, String texture, Material m)
    {
        super(id, fluid, m);
        this.texture = texture;
        setCreativeTab(TConstructRegistry.blockTab);
    }

    @Override
    public void registerIcons(IconRegister iconRegister)
    {
        this.stillIcon = iconRegister.registerIcon("projectred:compat/" + this.texture);
        this.getFluid().setStillIcon(stillIcon);

        this.flowIcon = iconRegister.registerIcon("projectred:compat/" + this.texture + "_flow");
        this.getFluid().setFlowingIcon(flowIcon);
    }

    @Override
    public Icon getIcon(int side, int meta)
    {
        if (side == 0 || side == 1)
            return this.stillIcon;
        return this.flowIcon;
    }

    @Override
    public void onEntityCollidedWithBlock(World par1World, int x, int y, int z, Entity entity)
    {
        if (entity instanceof EntityLivingBase)
        {
            entity.motionX *= 0.4D;
            entity.motionZ *= 0.4D;
        }
        if (!(entity instanceof EntityItem) && !entity.isImmuneToFire())
        {
            entity.attackEntityFrom(DamageSource.lava, 4.0F);
            entity.setFire(15);
        }
    }
}
