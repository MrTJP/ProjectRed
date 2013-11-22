package mrtjp.projectred.expansion;

import java.util.List;

import mrtjp.projectred.ProjectRedExpansion;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.JItemMultiPart;
import codechicken.multipart.MultiPartRegistry;
import codechicken.multipart.TMultiPart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemPartPipe extends JItemMultiPart {

    public ItemPartPipe(int id) {
        super(id);
        setHasSubtypes(true);
        setCreativeTab(ProjectRedExpansion.tabExpansion);
        setUnlocalizedName("projectred.expansion.pipe");
    }

    @Override
    public TMultiPart newPart(ItemStack item, EntityPlayer player, World world, BlockCoord pos, int side, Vector3 vhit) {
        EnumPipe type = EnumPipe.VALID_PIPE[item.getItemDamage()];
        BasicPipePart p = (BasicPipePart) MultiPartRegistry.createPart(type.type, false);
        
        if (p == null)
            return null;
        
        p.preparePlacement(item.getItemDamage());
        return p;
    }
    
    @Override
    public boolean onItemUse(ItemStack stack, EntityPlayer player, World w, int x, int y, int z, int side, float hitX, float hitY, float hitZ) {
        if (super.onItemUse(stack, player, w, x, y, z, side, hitX, hitY, hitZ)) {
            w.playSoundEffect(x + 0.5, y + 0.5, z + 0.5, Block.soundGlassFootstep.getPlaceSound(), (Block.soundGlassFootstep.getVolume() * 5.0F), Block.soundGlassFootstep.getPitch() * .9F);
            return true;
        }
        return false;
    }

    @Override
    public String getUnlocalizedName(ItemStack stack) {
        return super.getUnlocalizedName() + "|" + stack.getItemDamage();
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public void getSubItems(int id, CreativeTabs tab, List list) {
        for (EnumPipe t : EnumPipe.VALID_PIPE)
                list.add(t.getItemStack());
    }

    @Override
    public void registerIcons(IconRegister reg) {
        for (EnumPipe p : EnumPipe.VALID_PIPE)
            p.loadTextures(reg);
    }

    @Override
    @SideOnly(Side.CLIENT)
    public int getSpriteNumber() {
        return 0;
    }


}
