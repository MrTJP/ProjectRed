package mrtjp.projectred.illumination;

import java.util.List;

import mrtjp.projectred.ProjectRedIllumination;
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
import codechicken.multipart.TMultiPart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemPartLantern extends JItemMultiPart {

    public boolean inverted;
    
    public ItemPartLantern(int id, boolean isInverted) {
        super(id);
        inverted = isInverted;
        this.setHasSubtypes(true);
        this.setUnlocalizedName("projred.illumination.lantern");
        this.setCreativeTab(ProjectRedIllumination.tabLighting);
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
    public TMultiPart newPart(ItemStack is, EntityPlayer player, World w, BlockCoord pos, int side, Vector3 arg5) {
        BlockCoord onPos = pos.copy().offset(side ^ 1);
        if (!BasicWireUtils.canPlaceWireOnSide(w, onPos.x, onPos.y, onPos.z, ForgeDirection.getOrientation(side), false)) {
            return null;
        }
        return new LanternPart(EnumLantern.get(is.getItemDamage()), inverted, side ^ 1);
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void getSubItems(int id, CreativeTabs tab, List list) {
        for (int i = 0; i < 16; i++) {
            list.add(new ItemStack(this, 1, i));
        }
    }

    public String getUnlocalizedName(ItemStack stack) {
        return super.getUnlocalizedName() + (inverted ? "inv." : "") + "|" + stack.getItemDamage();
    }

    @Override
    public void registerIcons(IconRegister reg) {
        for (EnumLantern l : EnumLantern.values()) {
            l.registerIcon(reg);
        }
    }

    @Override
    @SideOnly(Side.CLIENT)
    public int getSpriteNumber() {
        return 0;
    }

}
