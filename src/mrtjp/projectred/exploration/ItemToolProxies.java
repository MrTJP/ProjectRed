package mrtjp.projectred.exploration;

import net.minecraft.item.ItemAxe;
import net.minecraft.item.ItemPickaxe;
import net.minecraft.item.ItemSpade;
import net.minecraft.item.ItemSword;

/**
 *  Proxy scala constructor calls to Java. Fixes weird compiler issue
 */
class ItemToolProxies {
	static class Axe extends ItemAxe {

		protected Axe(net.minecraft.item.Item.ToolMaterial material) {
			super(material);
		}
	}

	static class Pickaxe extends ItemPickaxe {

		protected Pickaxe(net.minecraft.item.Item.ToolMaterial material) {
			super(material);
		}
	}

	static class Shovel extends ItemSpade {

		protected Shovel(net.minecraft.item.Item.ToolMaterial material) {
			super(material);
		}
	}

	static class Sword extends ItemSword {

		protected Sword(net.minecraft.item.Item.ToolMaterial material) {
			super(material);
		}
	}
}
