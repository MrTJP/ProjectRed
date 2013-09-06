package mrtjp.projectred.illumination;

import java.util.List;

import mrtjp.projectred.ProjectRedIllumination;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.JItemMultiPart;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.minecraft.ButtonPart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemPartIlluminatedButton extends JItemMultiPart {
    
    public static Icon icons[];
    
    public ItemPartIlluminatedButton(int id) {
        super(id);
        this.setHasSubtypes(true);
        this.setUnlocalizedName("projectred.illumination.lightbutton");
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
            if(side == 0 || side == 1)
                return null;
            
            pos = pos.copy().offset(side^1);
            if(!w.isBlockSolidOnSide(pos.x, pos.y, pos.z, ForgeDirection.getOrientation(side)))
                return null;
            
            IlluminatedButtonPart b = new IlluminatedButtonPart(ButtonPart.sideMetaMap[side^1]|0<<4);
            if (b != null)
                b.onPlaced(is);
            return b;
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void getSubItems(int id, CreativeTabs tab, List list) {
        for (int i = 0; i < 16; i++) {
            list.add(new ItemStack(this, 1, i));
        }
    }

    public String getUnlocalizedName(ItemStack stack) {
        return super.getUnlocalizedName() + "|" + stack.getItemDamage();
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void registerIcons(IconRegister reg) {
        icons = new Icon[16];
        for (int i = 0; i < 16; i++)
            icons[i] = reg.registerIcon("projectred:lights/button/" + i);
    }

    @Override
    @SideOnly(Side.CLIENT)
    public int getSpriteNumber() {
        return 0;
    }
}
