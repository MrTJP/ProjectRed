package mrtjp.projectred.exploration;

import java.util.List;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.GhostContainer;
import mrtjp.projectred.core.GuiRestrictedSlot.ISlotCheck;
import mrtjp.projectred.core.SimpleInventory;
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
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemBackpack extends Item {

    public static Icon[] bpIcons = new Icon[EnumBackpack.VALID_BP.length];

    public ItemBackpack(int par1) {
        super(par1);
        hasSubtypes = true;
        maxStackSize = 1;
        setUnlocalizedName("projectred.exploration.backpack");
        setCreativeTab(ProjectRedExploration.tabExploration);
    }

    public static IInventory getBackpackInventory(EntityPlayer player) {
        SimpleInventory inv = null;
        ItemStack held = player.getHeldItem();
        if (held != null && held.itemID == ProjectRedExploration.itemBackpack.itemID) {
            inv = new BagInventory(player, held);
            inv.readFromNBT(held.getTagCompound(), "conents");
        }
        return inv;
    }

    public static Container getContainer(EntityPlayer player) {
        IInventory backpackInv = getBackpackInventory(player);
        if (backpackInv == null) {
            backpackInv = new BagInventory(player, new ItemStack(ProjectRedExploration.itemBackpack));
        }
        GhostContainer ghost = new GhostContainer(player.inventory, backpackInv);
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 9; j++) {
                final int slotNumber = i * 9 + j;
                ghost.addRestrictedSlot(slotNumber, backpackInv,  8 + (j * 18),  18 + (i * 18), new ISlotCheck() {

                    @Override
                    public boolean isSlotAllowed(ItemStack stack) {
                        if (stack == null) {
                            return true;
                        }
                        return !(stack.itemID == ProjectRedExploration.itemBackpack.itemID);
                    }
                    
                });
            }
        }
        ghost.addNormalSlotsForPlayerInventory(8, 86, player.inventory.currentItem);
        return ghost;
    }

    @Override
    public boolean onItemUse(ItemStack stack, EntityPlayer player, World world, int x, int y, int z, int par7, float par8, float par9, float par10) {
        openGui(player);
        return super.onItemUse(stack, player, world, x, y, z, par7, par8, par9, par10);
    }

    @Override
    public ItemStack onItemRightClick(final ItemStack stack, final World w, final EntityPlayer player) {
        openGui(player);
        return super.onItemRightClick(stack, w, player);
    }

    public void openGui(EntityPlayer player) {
        if (!player.isSneaking()) {
            player.openGui(ProjectRedExploration.instance, ExplorationGuiHandler.ID_Bag, player.worldObj, 0, 0, 0);
        }
    }

    @Override
    public void getSubItems(int id, CreativeTabs tab, List list) {
        for (EnumBackpack b : EnumBackpack.VALID_BP) {
            list.add(b.getItemStack());
        }
    }

    @Override
    public String getUnlocalizedName(ItemStack itemstack) {
        return this.getUnlocalizedName()+"."+EnumBackpack.get(itemstack.getItemDamage()).unlocalname;
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void registerIcons(IconRegister reg) {
        for (EnumBackpack b : EnumBackpack.VALID_BP) {
            bpIcons[b.meta] = reg.registerIcon("projectred:backpacks/" + b.unlocalname);
        }
    }

    /**
     * Gets an icon index based on an item's damage value
     */
    @Override
    public Icon getIconFromDamage(int meta) {
        if (meta > bpIcons.length - 1) {
            return null;
        }
        return bpIcons[meta];
    }

    public static class BagInventory extends SimpleInventory {
        public ItemStack _bagOriginal;
        public EntityPlayer _player;
        public boolean isLoading = false;

        public BagInventory(EntityPlayer player, ItemStack bag) {
            super(27, "bag", 64);
            _bagOriginal = bag;
            _player = player;
            loadInventory();
        }

        public void onInventoryChanged() {
            super.onInventoryChanged();
            if (!isLoading) {
                saveInventory();
            }
        }

        @Override
        public void openChest() {
            isLoading = true;
            loadInventory();
            isLoading = false;
        }

        @Override
        public void closeChest() {
            saveInventory();
        }

        private void loadInventory() {
            this.readFromNBT(_bagOriginal.getTagCompound());
        }

        private void saveInventory() {
            NBTTagCompound nbt = new NBTTagCompound();
            this.writeToNBT(nbt);
            _bagOriginal.setTagCompound(nbt);
            refreshNBT();
        }
        
        private void refreshNBT() {
            ItemStack currentBag = _player.getHeldItem();
            if (currentBag != null && currentBag.itemID == ProjectRedExploration.itemBackpack.itemID) {
                currentBag.setTagCompound(_bagOriginal.getTagCompound());
            }
        }
    }

    public enum EnumBackpack {
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
        public static final String oreDictDefinition = "pr_bag";
        
        private EnumBackpack(String name, String unlocal) {
            fullname = name;
            unlocalname = unlocal;
        }

        public static EnumBackpack get(int i) {
            if (i > VALID_BP.length - 1) {
                return WHITE;
            }
            return VALID_BP[i];
        }

        public ItemStack getItemStack() {
            return getItemStack(1);
        }

        public ItemStack getItemStack(int i) {
            return new ItemStack(ProjectRedExploration.itemBackpack, i, meta);
        }

        public static void initOreDictDefinitions() {
            for (EnumBackpack b : EnumBackpack.VALID_BP)
                OreDictionary.registerOre(oreDictDefinition, b.getItemStack());
        }
    }

}
