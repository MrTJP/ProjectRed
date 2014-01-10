package mrtjp.projectred.core;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;
import mrtjp.projectred.ProjectRedCore;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraftforge.oredict.OreDictionary;

import java.util.List;

public class ItemPart extends Item
{
    public static Icon[] partIcons = new Icon[EnumPart.VALID_PARTS.length];

    public ItemPart(int par1)
    {
        super(par1);
        setUnlocalizedName("projectred.core.part");
        setCreativeTab(ProjectRedCore.tabCore);
        setHasSubtypes(true);
    }

    /**
     * returns a list of items with the same ID, but different meta (eg: dye
     * returns 16 items)
     */
    @Override
    public void getSubItems(int id, CreativeTabs tab, List list)
    {
        for (EnumPart part : EnumPart.VALID_PARTS)
            list.add(part.getItemStack());
    }

    /**
     * Returns the unlocalized name of this item. This version accepts an
     * ItemStack so different stacks can have different names based on their
     * damage or NBT.
     */
    @Override
    public String getUnlocalizedName(ItemStack itemstack)
    {
        return getUnlocalizedName() + "." + EnumPart.get(itemstack.getItemDamage()).unlocalName;
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void registerIcons(IconRegister reg)
    {
        for (EnumPart part : EnumPart.VALID_PARTS)
            partIcons[part.meta] = reg.registerIcon("projectred:parts/" + part.unlocalName);
    }

    /**
     * Gets an icon index based on an item's damage value
     */
    @Override
    public Icon getIconFromDamage(int meta)
    {
        if (meta > partIcons.length - 1)
            return null;
        return partIcons[meta];
    }

    public enum EnumPart
    {
        PLATE("partplate"),
        CONDUCTIVEPLATE("partconductiveplate"),
        WIREDPLATE("partwiredplate"),
        BUNDLEDPLATE("partbundledplate"),
        ANODE("partanode"),
        CATHODE("partcathode"),
        POINTER("partpointer"),
        SILICONCHIP("partsiliconchip"),
        ENERGIZEDSILICONCHIP("partenergizedsiliconchip"),
        PLATFORMEDPLATE("partplatformedplate"),

        REDINGOT("partredingot"),
        SILICONBOULE("partboule"),
        SILICON("partsilicon"),
        INFUSEDSILICON("partinfusedsilicon"),
        ENERGIZEDSILICON("partenergizedsilicon"),
        MOTOR("partmotor"),
        COPPERCOIL("partcoppercoil"),
        IRONCOIL("partironcoil"),
        GOLDCOIL("partgoldcoil"),

        WHITEILLUMAR("illumar0"),
        ORANGEILLUMAR("illumar1"),
        MAGENTAILLUMAR("illumar2"),
        LIGHTBLUEILLUMAR("illumar3"),
        YELLOWILLUMAR("illumar4"),
        LIMEILLUMAR("illumar5"),
        PINKILLUMAR("illumar6"),
        GREYILLUMAR("illumar7"),
        LIGHTGREYILLUMAR("illumar8"),
        CYANILLUMAR("illumar9"),
        PURPLEILLUMAR("illumar10"),
        BLUEILLUMAR("illumar11"),
        BROWNILLUMAR("illumar12"),
        GREENILLUMAR("illumar13"),
        REDILLUMAR("illumar14"),
        BLACKILLUMAR("illumar15"),

        WOVENCLOTH("partcloth"),
        SAIL("partsail"),

        RUBY("gemruby"),
        SAPPHIRE("gemsapphire"),
        PERIDOT("gemperidot"),

        REDIRONCOMPOUND("partredironcomp"),
        SANDYCOALCOMPOUND("partsandcoalcomp"),
        REDSILICONCOMPOUND("partredsiliconcomp"),
        GLOWINGSILICONCOMPOUND("partglowsiliconcomp"),

        NULLROUTINGCHIP("nullchip"),
        NULLUPGRADECHIP("nullupgrd"),
        CHIPUPGRADE_LX("upgrd_lx"),
        CHIPUPGRADE_LY("upgrd_ly"),
        CHIPUPGRADE_LZ("upgrd_lz"),
        CHIPUPGRADE_RX("upgrd_rx"),
        CHIPUPGRADE_RY("upgrd_ry"),
        CHIPUPGRADE_RZ("upgrd_rz"),;

        public String unlocalName;
        public static final EnumPart[] VALID_PARTS = values();
        public static final EnumPart[] ILLUMAR_PARTS = {WHITEILLUMAR, ORANGEILLUMAR, MAGENTAILLUMAR, LIGHTBLUEILLUMAR, YELLOWILLUMAR, LIMEILLUMAR, PINKILLUMAR, GREYILLUMAR, LIGHTGREYILLUMAR, CYANILLUMAR, PURPLEILLUMAR, BLUEILLUMAR, BROWNILLUMAR, GREENILLUMAR, REDILLUMAR, BLACKILLUMAR};
        public int meta = this.ordinal();
        public static final String oreDictDefinition = "projredPart";
        public static final String oreDictDefinition_illumar = "projredIllumar";

        private EnumPart(String unlocal)
        {
            unlocalName = unlocal;
        }

        public static EnumPart get(int ordinal)
        {
            if (ordinal > VALID_PARTS.length - 1)
                return null;
            return VALID_PARTS[ordinal];
        }

        public ItemStack getItemStack()
        {
            return new ItemStack(ProjectRedCore.itemComponent, 1, meta);
        }

        public ItemStack getItemStack(int i)
        {
            return new ItemStack(ProjectRedCore.itemComponent, i, meta);
        }

        public static void initOreDictDefinitions()
        {
            for (EnumPart p : EnumPart.VALID_PARTS)
                OreDictionary.registerOre(oreDictDefinition, p.getItemStack());
            for (EnumPart p : EnumPart.ILLUMAR_PARTS)
                OreDictionary.registerOre(oreDictDefinition_illumar, p.getItemStack());
        }
    }
}
