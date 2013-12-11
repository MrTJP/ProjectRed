package mrtjp.projectred.integration;

import java.util.List;

import mrtjp.projectred.ProjectRedIntegration;
import mrtjp.projectred.core.BasicWireUtils;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.JItemMultiPart;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.TMultiPart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemPartGate extends JItemMultiPart
{
    public ItemPartGate(int id)
    {
        super(id);
        setHasSubtypes(true);
        setCreativeTab(ProjectRedIntegration.tabIntegration);
        setUnlocalizedName("projectred.integration.gate");
    }

    @Override
    public boolean onItemUse(ItemStack stack, EntityPlayer player, World w, int x, int y, int z, int side, float f, float f2, float f3)
    {
        if (super.onItemUse(stack, player, w, x, y, z, side, f, f2, f3))
        {
            w.playSoundEffect(x + 0.5, y + 0.5, z + 0.5, Block.soundGlassFootstep.getPlaceSound(), Block.soundGlassFootstep.getVolume() * 5.0F, Block.soundGlassFootstep.getPitch() * .8F);
            return true;
        }
        return false;
    }

    @Override
    public TMultiPart newPart(ItemStack item, EntityPlayer player, World world, BlockCoord pos, int side, Vector3 vhit)
    {
        BlockCoord onPos = pos.copy().offset(side ^ 1);
        if (!BasicWireUtils.canPlaceWireOnSide(world, onPos.x, onPos.y, onPos.z, ForgeDirection.getOrientation(side), false))
            return null;

        EnumGate type = EnumGate.VALID_GATES[item.getItemDamage()];
        if (!type.implemented())
            return null;
        GatePart gate = (GatePart) MultiPartRegistry.createPart(type.gateType, false);
        if (gate != null)
            gate.preparePlacement(player, pos, side, item.getItemDamage());
        return gate;
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void getSubItems(int id, CreativeTabs tab, List list)
    {
        for (EnumGate g : EnumGate.VALID_GATES)
            list.add(g.getItemStack());
    }

    @Override
    public String getUnlocalizedName(ItemStack stack)
    {
        return super.getUnlocalizedName() + "|" + stack.getItemDamage();
    }

    @Override
    public void registerIcons(IconRegister reg)
    {
        ComponentStore.registerIcons(reg);
    }

    @Override
    @SideOnly(Side.CLIENT)
    public int getSpriteNumber()
    {
        return 0;
    }
}
