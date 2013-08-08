package mrtjp.projectred.integration;

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

public class ItemPartGate extends JItemMultiPart {

    public ItemPartGate(int id) {
        super(id);
        setHasSubtypes(true);
        setCreativeTab(ProjectRedTabs.tabIntegration);
        setUnlocalizedName("projred.integration.gate");
    }

    @Override
    public boolean onItemUse(ItemStack stack, EntityPlayer player, World w, int x, int y, int z, int side, float f, float f2, float f3) {
        if (super.onItemUse(stack, player, w, x, y, z, side, f, f2, f3)) {
            w.playSoundEffect(x + 0.5, y + 0.5, z + 0.5, Block.soundGlassFootstep.getPlaceSound(), (Block.soundGlassFootstep.getVolume() * 5.0F), Block.soundGlassFootstep.getPitch() * .8F);
            return true;
        }
        return false;
    }

    @Override
    public TMultiPart newPart(ItemStack item, EntityPlayer player, World world, BlockCoord pos, int side, Vector3 vhit) {
        BlockCoord onPos = pos.copy().offset(side ^ 1);
        if (!world.isBlockSolidOnSide(onPos.x, onPos.y, onPos.z, ForgeDirection.getOrientation(side))) {
            return null;
        }
        GatePart gate = new GatePart(EnumGate.get(item.getItemDamage()));
        gate.setupPlacement(player, side);
        return gate;
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void getSubItems(int id, CreativeTabs tab, List list) {
        for (EnumGate g : EnumGate.VALUES) {
            list.add(g.getItemStack());
        }
    }

    public String getUnlocalizedName(ItemStack stack) {
        return super.getUnlocalizedName() + "|" + stack.getItemDamage();
    }

    @Override
    public void registerIcons(IconRegister reg) {
        GateRenderBridge.registerAllIcons(reg);

    }

    @Override
    @SideOnly(Side.CLIENT)
    public int getSpriteNumber() {
        return 0;
    }

}
