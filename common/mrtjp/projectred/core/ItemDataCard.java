package mrtjp.projectred.core;

import java.util.List;

import mrtjp.projectred.ProjectRedCore;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.EnumChatFormatting;
import net.minecraft.util.Icon;

import org.lwjgl.input.Keyboard;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemDataCard extends Item
{
    private Icon[] icons;

    public ItemDataCard(int par1)
    {
        super(par1);
        setUnlocalizedName("projectred.core.datacard");
        setCreativeTab(ProjectRedCore.tabCore);
        setMaxStackSize(1);
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void registerIcons(IconRegister reg)
    {
        icons = new Icon[2];
        icons[0] = reg.registerIcon("projectred:datacard0");
        icons[1] = reg.registerIcon("projectred:datacard1");
    }
    
    @Override
    public Icon getIcon(ItemStack stack, int pass)
    {
        return hasCardData(stack) ? icons[1] : icons[0];
    }
    
    @Override
    public Icon getIcon(ItemStack stack, int renderPass, EntityPlayer player, ItemStack usingItem, int useRemaining)
    {
        return hasCardData(stack) ? icons[1] : icons[0];
    }
    
    @Override
    public Icon getIconIndex(ItemStack stack)
    {
        return hasCardData(stack) ? icons[1] : icons[0];
    }
    
    @Override
    public void addInformation(ItemStack stack, EntityPlayer player, List list, boolean par4)
    {
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
        {
            if (hasCardData(stack))
            {
                NBTTagCompound root = stack.getTagCompound().getCompoundTag("carddata");
                for (Object base : root.getTags())
                    if (base instanceof NBTTagCompound)
                    {
                        NBTTagCompound tag = (NBTTagCompound) base; 
                        String s = tag.getString("title");
                        if (!s.isEmpty())
                        {
                            list.add(EnumChatFormatting.GRAY + s);
                            int infoCount = tag.getInteger("info_lines");
                            for (int i = 0; i < infoCount; i++)
                                list.add(EnumChatFormatting.GRAY + tag.getString("info"+i));
                        }
                    }
            }
            else
                list.add(EnumChatFormatting.GRAY + "no data");
        }
    }

    public static NBTTagCompound loadData(ItemStack stack, String directory)
    {
        if (stack.hasTagCompound())
        {
            NBTTagCompound root = stack.getTagCompound().getCompoundTag("carddata");

            return root.getCompoundTag(directory);
        }

        return new NBTTagCompound();
    }

    public static void saveData(ItemStack stack, NBTTagCompound tag, String directory)
    {
        if (stack == null || !(stack.getItem() instanceof ItemDataCard) || tag == null || directory == null || directory.isEmpty())
            return;
        
        NBTTagCompound stacktag = stack.hasTagCompound() ? stack.getTagCompound() : new NBTTagCompound();
        NBTTagCompound root = stacktag.getCompoundTag("carddata");
        
        root.setCompoundTag(directory, tag);
        stacktag.setCompoundTag("carddata", root);
        stack.setTagCompound(stacktag);
    }
    
    public static void deleteData(ItemStack stack, String directory)
    {
        if (hasCardData(stack))
        {
            NBTTagCompound stacktag = stack.getTagCompound();
            NBTTagCompound root = stacktag.getCompoundTag("carddata");
            root.removeTag(directory);
            
            stacktag.setCompoundTag("carddata", root);
            stack.setTagCompound(stacktag);
        }
    }
    
    public static boolean hasCardData(ItemStack stack)
    {
        if (stack != null && stack.getItem() instanceof ItemDataCard && stack.hasTagCompound())
            return !stack.getTagCompound().getCompoundTag("carddata").hasNoTags();
        
        return false;
    }
    
    public static void nameData(NBTTagCompound data, String title, String... info)
    {
        data.setString("title", title);
        data.setInteger("info_lines", info.length);
        
        for (int i = 0; i < info.length; i++)
            data.setString("info"+i, info[i]);
    }
}
