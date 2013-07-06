package mrtjp.projectred.multipart.wiring.gates;

import mrtjp.projectred.utils.Dir;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Vec3;
import net.minecraft.world.World;

public class ItemBlockGate extends ItemBlock {

	public ItemBlockGate(int id_minus_256) {
		super(id_minus_256);
		setHasSubtypes(true);
	}

	@Override
	public String getUnlocalizedName(ItemStack is) {
		return "projectred.gate." + is.getItemDamage();
	}

	@Override
	public boolean placeBlockAt(ItemStack stack, EntityPlayer player, World world, int x, int y, int z, int side, float hitX, float hitY, float hitZ, int metadata) {
		int meta = stack.getItemDamage();
		if (meta < 0 || meta >= EnumGate.VALUES.length)
			return false;

		if (!super.placeBlockAt(stack, player, world, x, y, z, side, hitX, hitY, hitZ, metadata))
			return false;

		int front = (side + 2) % 6; // unpredictable direction not parallel to
									// side

		Vec3 look = player.getLook(1.0f);
		double absx = Math.abs(look.xCoord);
		double absy = Math.abs(look.yCoord);
		double absz = Math.abs(look.zCoord);
		switch (side) {
		case Dir.PX:
		case Dir.NX:
			if (absy > absz)
				front = look.yCoord > 0 ? Dir.PY : Dir.NY;
			else
				front = look.zCoord > 0 ? Dir.PZ : Dir.NZ;
			break;
		case Dir.PY:
		case Dir.NY:
			if (absx > absz)
				front = look.xCoord > 0 ? Dir.PX : Dir.NX;
			else
				front = look.zCoord > 0 ? Dir.PZ : Dir.NZ;
			break;
		case Dir.PZ:
		case Dir.NZ:
			if (absy > absx)
				front = look.yCoord > 0 ? Dir.PY : Dir.NY;
			else
				front = look.xCoord > 0 ? Dir.PX : Dir.NX;
			break;
		}

		if (world.getBlockId(x, y, z) == itemID) {
			world.setBlockTileEntity(x, y, z, new TileGate(EnumGate.values()[meta], side ^ 1, front));
			world.notifyBlocksOfNeighborChange(x, y, z, itemID);
		}
		return true;
	}
}
