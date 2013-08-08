package mrtjp.projectred.transmission;

import java.util.List;

import mrtjp.projectred.core.ProjectRedTabs;
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
import codechicken.multipart.TMultiPart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemPartWire extends JItemMultiPart {

    public ItemPartWire(int id) {
        super(id);
        setHasSubtypes(true);
        setCreativeTab(ProjectRedTabs.tabTransmission);
        setUnlocalizedName("projred.transmission.wire");
    }

    @Override
    public boolean onItemUse(ItemStack stack, EntityPlayer player, World w, int x, int y, int z, int side, float f, float f2, float f3) {
        if (super.onItemUse(stack, player, w, x, y, z, side, f, f2, f3)) {
            w.playSoundEffect(x + 0.5, y + 0.5, z + 0.5, Block.soundGlassFootstep.getPlaceSound(), (Block.soundGlassFootstep.getVolume() * 5.0F), Block.soundGlassFootstep.getPitch() * .9F);
            return true;
        }
        return false;
    }

    @Override
    public TMultiPart newPart(ItemStack item, EntityPlayer player, World world, BlockCoord pos, int side, Vector3 vhit) {
        BlockCoord onPos = pos.copy().offset(side ^ 1);
        if (!BasicWireUtils.canPlaceWireOnSide(world, onPos.x, onPos.y, onPos.z, ForgeDirection.getOrientation(side), false)) {
            return null;
        }
        EnumWire w = EnumWire.VALID_WIRE[item.getItemDamage()];
        try {
            return (TMultiPart) w.wireClass.getConstructors()[0].newInstance(w, false, side ^ 1);
        } catch (Throwable e) {
            return null;
        }
    }


    @Override
    @SideOnly(Side.CLIENT)
    public void getSubItems(int id, CreativeTabs tab, List list) {
        for (EnumWire w : EnumWire.VALID_WIRE) {
            list.add(w.getItemStack());
        }
    }

    public String getUnlocalizedName(ItemStack stack) {
        return super.getUnlocalizedName() + "|" + stack.getItemDamage();
    }

    @Override
    public void registerIcons(IconRegister reg) {
        for (EnumWire wireType : EnumWire.VALID_WIRE) {
            wireType.loadTextures(reg);
        }
    }

    @Override
    @SideOnly(Side.CLIENT)
    public int getSpriteNumber() {
        return 0;
    }

}
