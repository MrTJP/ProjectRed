package mrtjp.projectred.exploration;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;
import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.ProjectRedExploration$;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.inventory.SpecialContainer;
import mrtjp.projectred.core.inventory.SpecialContainer.ISlotController.InventoryRulesController;
import mrtjp.projectred.core.inventory.SpecialContainer.SlotExtended;
import mrtjp.projectred.core.inventory.SimpleInventory;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import net.minecraft.world.World;
import net.minecraftforge.oredict.OreDictionary;

import java.util.List;

public class ItemBackpack extends Item
{
    public static Icon[] bpIcons = new Icon[EnumBackpack.VALID_BP.length];

    public ItemBackpack(int par1)
    {
        super(par1);
        hasSubtypes = true;
        maxStackSize = 1;
        setUnlocalizedName("projectred.exploration.backpack");
        setCreativeTab(ProjectRedExploration.tabExploration());
    }

    public static IInventory getBackpackInventory(EntityPlayer player)
    {
        ItemStack held = player.getHeldItem();
        if (held != null && held.itemID == ProjectRedExploration.itemBackpack().itemID)
        {
            return new BagInventory(player, held);
        }
        else return null;
    }

    public static Container getContainer(final EntityPlayer player)
    {
        IInventory backpackInv = getBackpackInventory(player);

        if (backpackInv == null)
            backpackInv = new BagInventory(player, new ItemStack(ProjectRedExploration.itemBackpack()));

        SpecialContainer container = new SpecialContainer(player.inventory) {
            @Override
            public SpecialContainer addCustomSlot(SlotExtended slot)
            {
                if (slot.getSlotIndex() == player.inventory.currentItem)
                    if (slot.inventory == player.inventory)
                        return super.addCustomSlot(slot.setRemoval(false));

                return super.addCustomSlot(slot);
            }
        };

        for (int i = 0; i < 3; i++)
            for (int j = 0; j < 9; j++)
            {
                int slotNumber = i*9+j;
                int x = 8+j*18;
                int y = 18+i*18;

                SlotExtended s = new SlotExtended(backpackInv, slotNumber, x, y).setCheck(InventoryRulesController.instance);

                container.addCustomSlot(s);
            }

        container.addPlayerInventory(8,  86);
        return container;
    }

    @Override
    public boolean onItemUse(ItemStack stack, EntityPlayer player, World world, int x, int y, int z, int par7, float par8, float par9, float par10)
    {
        openGui(player);
        return super.onItemUse(stack, player, world, x, y, z, par7, par8, par9, par10);
    }

    @Override
    public ItemStack onItemRightClick(final ItemStack stack, final World w, final EntityPlayer player)
    {
        openGui(player);
        return super.onItemRightClick(stack, w, player);
    }

    public void openGui(EntityPlayer player)
    {
        if (!player.isSneaking())
            player.openGui(ProjectRedExploration$.MODULE$, ExplorationProxy.ID_Bag(), player.worldObj, 0, 0, 0);
    }

    @Override
    public void getSubItems(int id, CreativeTabs tab, List list)
    {
        for (EnumBackpack b : EnumBackpack.VALID_BP)
            list.add(b.getItemStack());
    }

    @Override
    public String getUnlocalizedName(ItemStack itemstack)
    {
        return this.getUnlocalizedName() + "." + EnumBackpack.get(itemstack.getItemDamage()).unlocalname;
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void registerIcons(IconRegister reg)
    {
        for (EnumBackpack b : EnumBackpack.VALID_BP)
            bpIcons[b.meta] = reg.registerIcon("projectred:backpacks/" + b.unlocalname);
    }

    /**
     * Gets an icon index based on an item's damage value
     */
    @Override
    public Icon getIconFromDamage(int meta)
    {
        if (meta > bpIcons.length - 1)
            return null;
        return bpIcons[meta];
    }

    public static class BagInventory extends SimpleInventory
    {
        public ItemStack bag;
        public EntityPlayer player;

        public BagInventory(EntityPlayer player, ItemStack bag)
        {
            super(27, "bag", 64);
            this.bag = bag;
            this.player = player;
            loadInventory();
        }

        private void loadInventory()
        {
            assertNBT(bag);
            if (bag.getTagCompound().getBoolean("notLegacy"))//legacy loading TODO remove at some point
                load(bag.getTagCompound().getCompoundTag("baginv"));
            else load(bag.getTagCompound());
        }

        private void saveInventory()
        {
            NBTTagCompound nbt = new NBTTagCompound();
            save(nbt);
            assertNBT(bag);
            bag.getTagCompound().setCompoundTag("baginv", nbt);
            bag.getTagCompound().setBoolean("notLegacy", true); //legacy loading TODO remove at some point
            refreshNBT();
        }

        private void refreshNBT()
        {
            ItemStack currentBag = player.getHeldItem();
            if (currentBag != null && currentBag.itemID == ProjectRedExploration.itemBackpack().itemID)
                currentBag.setTagCompound(bag.getTagCompound());
        }

        @Override
        public void markDirty()
        {
            super.markDirty();
            saveInventory();
        }

        @Override
        public boolean isItemValidForSlot(int i, ItemStack stack)
        {
            if (stack != null)
            {
                if (stack.itemID == ProjectRedExploration.itemBackpack().itemID)
                    return false;

                for (int blocked : Configurator.backpackBlacklist)
                    if (stack.itemID == blocked)
                        return false;

                return true;
            }
            return false;
        }

        private void assertNBT(ItemStack s)
        {
            if (!s.hasTagCompound()) s.setTagCompound(new NBTTagCompound());
        }
    }

    public enum EnumBackpack
    {
        WHITE("White Backpack", "bpwhite"),
        ORANGE("Orange Backpack", "bporange"),
        MAGENTA("Magenta Backpack", "bpmagenta"),
        LIGHT_BLUE("Light Blue Backpack", "bplightblue"),
        YELLOW("Yellow Backpack", "bpyellow"),
        LIME("Lime Backpack", "bplime"),
        PINK("Pink Backpack", "bppink"),
        GREY("Grey Backpack", "bpgrey"),
        LIGHT_GREY("Light Grey Backpack", "bplightgrey"),
        CYAN("Cyan Backpack", "bpcyan"),
        PURPLE("Purple Backpack", "bppurple"),
        BLUE("Blue Backpack", "bpblue"),
        BROWN("Brown Backpack", "bpbrown"),
        GREEN("Green Backpack", "bpgreen"),
        RED("Red Backpack", "bpred"),
        BLACK("Black Backpack", "bpblack"), ;

        public final String fullname;
        public final String unlocalname;
        public final int meta = this.ordinal();
        public static final EnumBackpack[] VALID_BP = { WHITE, ORANGE, MAGENTA, LIGHT_BLUE, YELLOW, LIME, PINK, GREY, LIGHT_GREY, CYAN, PURPLE, BLUE, BROWN, GREEN, RED, BLACK };
        public static final String oreDictDefinition = "ProjRed|Exploration:bag";

        private EnumBackpack(String name, String unlocal)
        {
            fullname = name;
            unlocalname = unlocal;
        }

        public static EnumBackpack get(int i)
        {
            if (i > VALID_BP.length - 1)
                return WHITE;
            return VALID_BP[i];
        }

        public ItemStack getItemStack()
        {
            return getItemStack(1);
        }

        public ItemStack getItemStack(int i)
        {
            return new ItemStack(ProjectRedExploration.itemBackpack(), i, meta);
        }

        public static void initOreDictDefinitions()
        {
            for (EnumBackpack b : EnumBackpack.VALID_BP)
                OreDictionary.registerOre(oreDictDefinition, b.getItemStack());
        }
    }
}
