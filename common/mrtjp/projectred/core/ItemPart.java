package mrtjp.projectred.core;

import java.util.List;

import mrtjp.projectred.ProjectRedCore;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraftforge.oredict.OreDictionary;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

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
        PLATE("Circuit Plate", "partplate"),
        CONDUCTIVEPLATE("Conductive Plate", "partconductiveplate"),
        WIREDPLATE("Wired Plate", "partwiredplate"),
        BUNDLEDPLATE("Bundled Plate", "partbundledplate"),
        ANODE("Anode", "partanode"),
        CATHODE("Cathode", "partcathode"),
        POINTER("Pointer", "partpointer"),
        SILICONCHIP("Silicon Chip", "partsiliconchip"),
        ENERGIZEDSILICONCHIP("Energized Silicon Chip", "partenergizedsiliconchip"),
        PLATFORMEDPLATE("Platformed Plate", "partplatformedplate"),

        REDINGOT("Red Alloy Ingot", "partredingot"),
        SILICONBOULE("Silicon Boule", "partboule"),
        SILICON("Silicon", "partsilicon"),
        INFUSEDSILICON("Infused Silicon", "partinfusedsilicon"),
        ENERGIZEDSILICON("Energized Silicon", "partenergizedsilicon"),
        MOTOR("Motor", "partmotor"),
        COPPERCOIL("Copper Coil", "partcoppercoil"),
        IRONCOIL("Iron Coil", "partironcoil"),
        GOLDCOIL("Golden Coil", "partgoldcoil"),

        WHITEILLUMAR("White Illumar", "illumar0"),
        ORANGEILLUMAR("Orange Illumar", "illumar1"),
        MAGENTAILLUMAR("Magenta Illumar", "illumar2"),
        LIGHTBLUEILLUMAR("Light Blue Illumar", "illumar3"),
        YELLOWILLUMAR("Yellow Illumar", "illumar4"),
        LIMEILLUMAR("Lime Illumar", "illumar5"),
        PINKILLUMAR("Pink Illumar", "illumar6"),
        GREYILLUMAR("Grey Illumar", "illumar7"),
        LIGHTGREYILLUMAR("Light Grey Illumar", "illumar8"),
        CYANILLUMAR("Cyan Illumar", "illumar9"),
        PURPLEILLUMAR("Purple Illumar", "illumar10"),
        BLUEILLUMAR("Blue Illumar", "illumar11"),
        BROWNILLUMAR("Brown Illumar", "illumar12"),
        GREENILLUMAR("Green Illumar", "illumar13"),
        REDILLUMAR("Red Illumar", "illumar14"),
        BLACKILLUMAR("Black Illumar", "illumar15"),

        WOVENCLOTH("Woven Cloth", "partcloth"),
        SAIL("Sail", "partsail"),

        RUBY("Ruby", "gemruby"),
        SAPPHIRE("Sapphire", "gemsapphire"),
        PERIDOT("Peridot", "gemperidot"),

        REDIRONCOMPOUND("Red Iron Compound", "partredironcomp"),
        SANDYCOALCOMPOUND("Sandy coal compound", "partsandcoalcomp"),
        REDSILICONCOMPOUND("Red silicon compound", "partredsiliconcomp"),
        GLOWINGSILICONCOMPOUND("Glowing silicon compound", "partglowsiliconcomp"),

        NULLROUTINGCHIP("Null Routing Chip", "nullchip"), 
        ;
        
        public String fullName;
        public String unlocalName;
        public static final EnumPart[] VALID_PARTS = values();
        public static final EnumPart[] ILLUMAR_PARTS = { WHITEILLUMAR, ORANGEILLUMAR, MAGENTAILLUMAR, LIGHTBLUEILLUMAR, YELLOWILLUMAR, LIMEILLUMAR, PINKILLUMAR, GREYILLUMAR, LIGHTGREYILLUMAR, CYANILLUMAR, PURPLEILLUMAR, BLUEILLUMAR, BROWNILLUMAR, GREENILLUMAR, REDILLUMAR, BLACKILLUMAR };
        public int meta = this.ordinal();
        public static final String oreDictDefinition = "projredPart";
        public static final String oreDictDefinition_illumar = "projredIllumar";

        private EnumPart(String full, String unlocal)
        {
            fullName = full;
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
