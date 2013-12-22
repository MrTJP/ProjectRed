package mrtjp.projectred.transportation;

import java.util.List;

import mrtjp.projectred.ProjectRedTransportation;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.EnumChatFormatting;
import net.minecraft.util.Icon;
import net.minecraft.world.World;

import org.lwjgl.input.Keyboard;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemRoutingChip extends Item
{
    public ItemRoutingChip(int par1)
    {
        super(par1);
        setUnlocalizedName("projectred.transportation.routingchip");
        setCreativeTab(ProjectRedTransportation.tabTransportation);
        setHasSubtypes(true);
    }

    @Override
    public void getSubItems(int id, CreativeTabs tab, List list)
    {
        for (EnumRoutingChip c : EnumRoutingChip.VALID_CHIPS)
            list.add(c.getItemStack());
    }

    @Override
    public String getUnlocalizedName(ItemStack stack)
    {
        return getUnlocalizedName() + "|" + stack.getItemDamage();
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void registerIcons(IconRegister reg)
    {
        for (EnumRoutingChip c : EnumRoutingChip.VALID_CHIPS)
            c.registerIcons(reg);
    }

    @Override
    public Icon getIconFromDamage(int meta)
    {
        return EnumRoutingChip.VALID_CHIPS[meta].icon;
    }

    @Override
    public void addInformation(ItemStack stack, EntityPlayer player, List list, boolean par4)
    {
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
            if (stack.hasTagCompound())
            {
                RoutingChipset r = loadChipFromItemStack(stack);
                if (r != null)
                {
                    List<String> s = r.infoCollection();
                    if (s != null)
                        list.addAll(s);
                }
            }
            else
                list.add(EnumChatFormatting.GRAY + "not configured");
    }

    @Override
    public ItemStack onItemRightClick(ItemStack stack, World w, EntityPlayer player)
    {
        if (!w.isRemote && stack != null && stack.getItem() instanceof ItemRoutingChip)
        {
            RoutingChipset r = loadChipFromItemStack(stack);
            if (r != null)
                r.openGui(player);
        }
        return super.onItemRightClick(stack, w, player);
    }

    @Override
    public boolean onItemUse(ItemStack stack, EntityPlayer player, World w, int par4, int par5, int par6, int par7, float par8, float par9, float par10)
    {
        if (!w.isRemote && stack != null && stack.getItem() instanceof ItemRoutingChip)
        {
            RoutingChipset r = loadChipFromItemStack(stack);
            if (r != null)
                r.openGui(player);
        }
        return true;
    }
    
    @Override
    public boolean shouldPassSneakingClickToBlock(World par2World, int par4, int par5, int par6)
    {
        return true;
    }

    public static void saveChipToItemStack(ItemStack stack, RoutingChipset chipset)
    {
        if (stack == null || chipset == null || !(stack.getItem() instanceof ItemRoutingChip))
            return;

        NBTTagCompound mainTag = new NBTTagCompound("main");
        NBTTagCompound chipTag = new NBTTagCompound("ROM");
        chipset.save(chipTag);

        mainTag.setTag("chipROM", chipTag);

        stack.setTagCompound(mainTag);
    }

    public static RoutingChipset loadChipFromItemStack(ItemStack stack)
    {
        if (stack == null || !(stack.getItem() instanceof ItemRoutingChip))
            return null;

        EnumRoutingChip e = EnumRoutingChip.get(stack.getItemDamage());
        if (e != null)
        {
            RoutingChipset chip = e.createChipset();

            NBTTagCompound mainTag = stack.getTagCompound();
            if (mainTag != null && mainTag.hasKey("chipROM"))
                chip.load(mainTag.getCompoundTag("chipROM"));

            return chip;
        }
        return null;
    }

    public enum EnumRoutingChip
    {
        ITEMRESPONDER("responder", RoutingChipset_ItemResponder.class),
        DYNAMICITEMRESPONDER("responder_dyn", RoutingChipset_DynamicItemResponder.class),
        ITEMOVERFLOWRESPONDER("overflow", RoutingChipset_ItemOverflowResponder.class),
        ITEMTERMINATOR("terminator", RoutingChipset_ItemTerminator.class),
        ITEMEXTRACTOR("extractor", RoutingChipset_ItemExtractor.class),
        ITEMBROADCASTER("broadcaster", RoutingChipset_ItemBroadcaster.class),
        ITEMSTOCKKEEPER("stockkeeper", RoutingChipset_ItemStockKeeper.class), ;

        public static final EnumRoutingChip[] VALID_CHIPS = values();
        private final String iconPath;
        public final Class<? extends RoutingChipset> chipset;

        public final int meta = this.ordinal();
        public Icon icon;

        private EnumRoutingChip(String iconPath, Class<? extends RoutingChipset> chipset)
        {
            this.iconPath = "projectred:chips/" + iconPath;
            this.chipset = chipset;
        }

        public RoutingChipset createChipset()
        {
            try
            {
                return chipset.getConstructor(new Class[] {}).newInstance(new Object[] {});
            } catch (Throwable t)
            {
                t.printStackTrace();
                return null;
            }
        }

        public void registerIcons(IconRegister reg)
        {
            icon = reg.registerIcon(iconPath);
        }

        public ItemStack getItemStack()
        {
            return getItemStack(1);
        }
        
        public ItemStack getItemStack(int i)
        {
            return new ItemStack(ProjectRedTransportation.itemRoutingChip, i, meta);
        }

        public static EnumRoutingChip getForStack(ItemStack stack)
        {
            if (stack != null && stack.getItem() instanceof ItemRoutingChip)
                return get(stack.getItemDamage());
            
            return null;
        }
        
        public static EnumRoutingChip get(int ordinal)
        {
            if (ordinal < 0 || ordinal >= VALID_CHIPS.length)
                return null;

            return VALID_CHIPS[ordinal];
        }
    }
}
