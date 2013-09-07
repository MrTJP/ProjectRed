package mrtjp.projectred.illumination;

import java.util.Arrays;

import mrtjp.projectred.ProjectRedIllumination;
import net.minecraft.block.Block;
import net.minecraft.block.BlockButton;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.minecraft.ButtonPart;
import codechicken.multipart.minecraft.PartMetaAccess;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class IlluminatedButtonPart extends ButtonPart implements ILight {

    public byte colorMeta;

    public IlluminatedButtonPart() {}
    
    public IlluminatedButtonPart(int meta) {
        super(meta);
    }
    
    public void onPlaced(ItemStack is) {
        colorMeta = (byte) is.getItemDamage();
    }

    @Override
    public boolean isOn() {
        return pressed();
    }

    public static BlockButton getButton(int meta) {
        return stoneButton;
    }

    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        tag.setByte("colorMeta", colorMeta);
    }

    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        colorMeta = tag.getByte("colorMeta");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(colorMeta);
    }

    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        colorMeta = packet.readByte();
    }
    
    @Override
    public String getType() {
        return "pr_lightbutton";
    }
    
    @Override
    public Iterable<ItemStack> getDrops() {
        return Arrays.asList(getItemStack());
    }
    
    @Override
    public ItemStack pickItem(MovingObjectPosition hit) {
        return getItemStack();
    }

    public ItemStack getItemStack() {
        return new ItemStack(ProjectRedIllumination.itemPartIlluminatedButton, 1, colorMeta);
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass) {
        if (pass == 0) {
            RenderBlocks r = new RenderBlocks(new PartMetaAccess(this));
            r.renderBlockUsingTexture(getBlock(), x(), y(), z(), ItemPartIlluminatedButton.icons[colorMeta]);
            if (pressed()) {
                Cuboid6 box = getBounds().expand(0.025D);
                LastEventBasedHaloRenderer.addLight(x(), y(), z(), colorMeta, getFace(), box);
            }
        }
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public Icon getBrokenIcon(int side) {
        return ItemPartIlluminatedButton.icons[colorMeta];
    }
    
    @Override
    public Icon getBreakingIcon(Object subPart, int side) {
        return getBrokenIcon(side);
    }
    
    @Override
    public int getLightValue() {
        return pressed() ? 5 : 0;
    }

}
