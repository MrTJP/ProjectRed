package mrtjp.projectred.items;

import java.util.List;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.crafting.ProjectRedTabs;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraftforge.oredict.OreDictionary;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemPart extends Item {

	public static Icon[] partIcons = new Icon[EnumPart.VALID_PARTS.length];

	public ItemPart(int par1) {
		super(par1);
		setCreativeTab(ProjectRedTabs.tabParts);
		setHasSubtypes(true);
	}

	/**
	 * returns a list of items with the same ID, but different meta (eg: dye
	 * returns 16 items)
	 */
	@Override
	public void getSubItems(int id, CreativeTabs tab, List list) {
		for (EnumPart part : EnumPart.VALID_PARTS) {
			list.add(part.getItemStack());
		}
	}

	/**
	 * Returns the unlocalized name of this item. This version accepts an
	 * ItemStack so different stacks can have different names based on their
	 * damage or NBT.
	 */
	@Override
	public String getUnlocalizedName(ItemStack itemstack) {
		return EnumPart.get(itemstack.getItemDamage()).unlocalName;
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void registerIcons(IconRegister reg) {
		for (EnumPart part : EnumPart.VALID_PARTS) {
			partIcons[part.meta] = reg.registerIcon("projectred:parts/" + part.unlocalName);
		}
	}

	/**
	 * Gets an icon index based on an item's damage value
	 */
	@Override
	public Icon getIconFromDamage(int meta) {
		if (meta > partIcons.length - 1) {
			return null;
		}
		return partIcons[meta];
	}

	public enum EnumPart {
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

		WHITEILLUMAR("White Illumar", "partwhitedust"),
		ORANGEILLUMAR("Orange Illumar", "partorangedust"),
		MAGENTAILLUMAR("Magenta Illumar", "partmagentadust"),
		LIGHTBLUEILLUMAR("Light Blue Illumar", "partlightbluedust"),
		YELLOWILLUMAR("Yellow Illumar", "partyellowdust"),
		LIMEILLUMAR("Lime Illumar", "partlimedust"),
		PINKILLUMAR("Pink Illumar", "partpinkdust"),
		GREYILLUMAR("Grey Illumar", "partgreydust"),
		LIGHTGREYILLUMAR("Light Grey Illumar", "partlightgreydust"),
		CYANILLUMAR("Cyan Illumar", "partcyandust"),
		PURPLEILLUMAR("Purple Illumar", "partpurpledust"),
		BLUEILLUMAR("Blue Illumar", "partbluedust"),
		BROWNILLUMAR("Brown Illumar", "partbrowndust"),
		GREENILLUMAR("Green Illumar", "partgreendust"),
		REDILLUMAR("Red Illumar", "partreddust"),
		BLACKILLUMAR("Black Illumar", "partblackdust"),

		WOVENCLOTH("Woven Cloth", "partcloth"),
		SAIL("Sail", "partsail"),
		;
		public String fullName;
		public String unlocalName;
		public static final EnumPart[] VALID_PARTS = values();
		public static final EnumPart[] ILLUMAR_PARTS = { WHITEILLUMAR, ORANGEILLUMAR, MAGENTAILLUMAR, LIGHTBLUEILLUMAR, YELLOWILLUMAR, LIMEILLUMAR, PINKILLUMAR, GREYILLUMAR, LIGHTGREYILLUMAR, CYANILLUMAR, PURPLEILLUMAR, BLUEILLUMAR, BROWNILLUMAR, GREENILLUMAR, REDILLUMAR, BLACKILLUMAR };
		public int meta = this.ordinal();
		public static final String oreDictDefinition = "projredPart";
		public static final String oreDictDefinition_illumar = "projredIllumar";

		private EnumPart(String full, String unlocal) {
			fullName = full;
			unlocalName = unlocal;
		}

		public static EnumPart get(int ordinal) {
			if (ordinal > VALID_PARTS.length - 1) {
				return null;
			}
			return VALID_PARTS[ordinal];
		}

		public ItemStack getItemStack() {
			return new ItemStack(ProjectRed.itemComponent, 1, meta);
		}

		public ItemStack getItemStack(int i) {
			return new ItemStack(ProjectRed.itemComponent, i, meta);
		}

		public static void initOreDictDefinitions() {
			for (EnumPart p : EnumPart.VALID_PARTS) {
				OreDictionary.registerOre(oreDictDefinition, p.getItemStack());
			}
			for (EnumPart p : EnumPart.ILLUMAR_PARTS) {
				OreDictionary.registerOre(oreDictDefinition_illumar, p.getItemStack());
			}
		}
	}
}
