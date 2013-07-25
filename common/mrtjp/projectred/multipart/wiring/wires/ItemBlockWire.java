package mrtjp.projectred.multipart.wiring.wires;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.multipart.microblocks.EnumPosition;
import mrtjp.projectred.multipart.microblocks.IMicroblockCoverSystem;
import mrtjp.projectred.multipart.microblocks.IMicroblockSupporterTile;
import mrtjp.projectred.multipart.microblocks.Part;
import mrtjp.projectred.multipart.wiring.wires.EnumWire.WireDamageValues;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.BasicWireUtils;
import mrtjp.projectred.utils.Dir;
import net.minecraft.block.Block;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;

public class ItemBlockWire extends ItemBlock {

	public ItemBlockWire(int id_minus_256) {
		super(id_minus_256);
		setHasSubtypes(true);
	}

	@Override
	public String getUnlocalizedName(ItemStack is) {
		int damage = is.getItemDamage();
		boolean jacketed = WireDamageValues.isJacketed(damage);
		EnumWire type = WireDamageValues.getType(damage);
		if (type == null)
			return "item." + ".invalid";

		String name = "item.projectred.wire." + type.name().toLowerCase().replace('_', '-');

		if (jacketed)
			name += ".j";

		return name;
	}

	@Override
	public boolean onItemUse(ItemStack stack, EntityPlayer ply, World w, int x, int y, int z, int side, float hitX, float hitY, float hitZ) {
		int var11 = w.getBlockId(x, y, z);
		EnumWire type = WireDamageValues.getType(stack.getItemDamage());
		if (type == null)
			return false;

		boolean jacketed = WireDamageValues.isJacketed(stack.getItemDamage());

		if (!jacketed) {

			// placing normal wire

			if (!BasicWireUtils.canPlaceWireOnSide(w, x, y, z, ForgeDirection.VALID_DIRECTIONS[side], false)) {
				return false;
			}

			boolean mergeIntoWireTile = false;
			boolean mergeIntoMicroblockTile = false;

			Block microblockContainerBlock = ProjectRed.blockMicrocontainer;

			if (var11 == Block.snow.blockID)
				side = 1;
			else if (!mergeIntoWireTile && var11 != Block.vine.blockID && var11 != Block.tallGrass.blockID && var11 != Block.deadBush.blockID && (Block.blocksList[var11] == null || !Block.blocksList[var11].isBlockReplaceable(w, x, y, z))) {
				switch (side) {
				case Dir.NX:
					x--;
					break;
				case Dir.PX:
					x++;
					break;
				case Dir.NY:
					y--;
					break;
				case Dir.PY:
					y++;
					break;
				case Dir.NZ:
					z--;
					break;
				case Dir.PZ:
					z++;
					break;
				}

				mergeIntoWireTile = w.getBlockId(x, y, z) == ProjectRed.blockWire.blockID && ((TileWire) w.getBlockTileEntity(x, y, z)).canPlaceWireOnSide(type, side ^ 1);
				mergeIntoMicroblockTile = microblockContainerBlock != null && w.getBlockId(x, y, z) == microblockContainerBlock.blockID && !((IMicroblockSupporterTile) w.getBlockTileEntity(x, y, z)).getCoverSystem().isPositionOccupied(EnumPosition.getFacePosition(side ^ 1));
			}

			if (stack.stackSize == 0)
				return false;
			else if (!ply.canPlayerEdit(x, y, z, side, stack))
				return false;
			else if (mergeIntoWireTile) {
				if (w.isRemote)
					return true;

				TileWire wt = (TileWire) w.getBlockTileEntity(x, y, z);

				if (wt.addWire(type, side ^ 1)) {
					Block var12 = Block.blocksList[itemID];
					w.playSoundEffect(x + 0.5, y + 0.5, z + 0.5, Block.soundGlassFootstep.getPlaceSound(), (Block.soundGlassFootstep.getVolume() + 1.0F) / 2.0F, Block.soundGlassFootstep.getPitch() * 0.8F);
					--stack.stackSize;
				}

				return true;

			} else if (mergeIntoMicroblockTile) {
				if (w.isRemote)
					return true;

				IMicroblockSupporterTile oldTile = (IMicroblockSupporterTile) w.getBlockTileEntity(x, y, z);

				TileWire tile;

				try {
					tile = type.teclass.getConstructor().newInstance();
				} catch (Exception e) {
					throw new RuntimeException(e);
				}

				tile.rawAddWire(type, side ^ 1);
				for (Part p : oldTile.getCoverSystem().getAllParts())
					tile.getCoverSystem().addPart(p);

				// don't cause block or client update before tile is set
				w.setBlock(x, y, z, ProjectRed.blockWire.blockID, EnumWire.CLASS_TO_META.get(tile.getClass()), 0);
				w.setBlockTileEntity(x, y, z, tile);
				w.markBlockForUpdate(x, y, z);

				Block var12 = Block.blocksList[itemID];
				w.playSoundEffect(x + 0.5, y + 0.5, z + 0.5, Block.soundGlassFootstep.getPlaceSound(), (Block.soundGlassFootstep.getVolume() + 1.0F) / 2.0F, Block.soundGlassFootstep.getPitch() * 0.8F);
				--stack.stackSize;

				tile.notifyExtendedNeighbours();

				return true;

			} else if (w.canPlaceEntityOnSide(this.itemID, x, y, z, false, side, ply, stack)) {
				if (w.isRemote)
					return true;

				Block var12 = Block.blocksList[this.itemID];
				int var13 = this.getMetadata(stack.getItemDamage());

				if (placeBlockAt(stack, ply, w, x, y, z, side, hitX, hitY, hitZ, var13)) {
					w.playSoundEffect(x + 0.5, y + 0.5, z + 0.5, Block.soundGlassFootstep.getPlaceSound(), (Block.soundGlassFootstep.getVolume() + 1.0F) / 2.0F, Block.soundGlassFootstep.getPitch() * 0.8F);
					--stack.stackSize;
				}

				return true;
			} else {
				return false;
			}

		} else {
			// placing jacketed wire

			boolean mergeIntoWireTile = false;
			boolean mergeIntoMicroblockTile = false;

			Block microblockContainerBlock = ProjectRed.blockMicrocontainer;

			if (w.getBlockId(x, y, z) == ProjectRed.blockWire.blockID) {
				TileWire wt = (TileWire) w.getBlockTileEntity(x, y, z);
				if (wt.canAddJacketedWire(type)) {
					mergeIntoWireTile = true;
				}
			}

			if (!mergeIntoWireTile && var11 != Block.snow.blockID && var11 != Block.vine.blockID && var11 != Block.tallGrass.blockID && var11 != Block.deadBush.blockID && (Block.blocksList[var11] == null || !Block.blocksList[var11].isBlockReplaceable(w, x, y, z))) {
				switch (side) {
				case Dir.NX:
					x--;
					break;
				case Dir.PX:
					x++;
					break;
				case Dir.NY:
					y--;
					break;
				case Dir.PY:
					y++;
					break;
				case Dir.NZ:
					z--;
					break;
				case Dir.PZ:
					z++;
					break;
				}
			}

			mergeIntoMicroblockTile = microblockContainerBlock != null && w.getBlockId(x, y, z) == microblockContainerBlock.blockID;
			if (mergeIntoMicroblockTile) {

				IMicroblockCoverSystem imcs = ((IMicroblockSupporterTile) w.getBlockTileEntity(x, y, z)).getCoverSystem();
				if (imcs.isPositionOccupied(EnumPosition.Centre) || imcs.isPositionOccupied(EnumPosition.PostX) || imcs.isPositionOccupied(EnumPosition.PostY) || imcs.isPositionOccupied(EnumPosition.PostZ))
					mergeIntoMicroblockTile = false;
			}

			if (stack.stackSize == 0)
				return false;
			else if (!ply.canPlayerEdit(x, y, z, side, stack))
				return false;
			else if (mergeIntoWireTile) {
				if (w.isRemote)
					return true;

				TileWire wt = (TileWire) w.getBlockTileEntity(x, y, z);

				if (wt.addJacketedWire(type)) {
					Block var12 = Block.blocksList[itemID];
					w.playSoundEffect(x + 0.5, y + 0.5, z + 0.5, Block.soundGlassFootstep.getPlaceSound(), (Block.soundGlassFootstep.getVolume() + 1.0F) / 2.0F, Block.soundGlassFootstep.getPitch() * 0.8F);
					--stack.stackSize;
				}

				return true;

			} else if (mergeIntoMicroblockTile) {
				if (w.isRemote)
					return true;

				IMicroblockSupporterTile oldTile = (IMicroblockSupporterTile) w.getBlockTileEntity(x, y, z);

				TileWire tile;

				try {
					tile = type.teclass.getConstructor().newInstance();
				} catch (Exception e) {
					throw new RuntimeException(e);
				}

				tile.rawAddJacketedWire(type);
				for (Part p : oldTile.getCoverSystem().getAllParts())
					tile.getCoverSystem().addPart(p);

				// don't cause block or client update before tile is set
				w.setBlock(x, y, z, ProjectRed.blockWire.blockID, EnumWire.CLASS_TO_META.get(tile.getClass()), 0);
				w.setBlockTileEntity(x, y, z, tile);
				w.markBlockForUpdate(x, y, z);

				Block var12 = Block.blocksList[itemID];
				w.playSoundEffect(x + 0.5, y + 0.5, z + 0.5, Block.soundGlassFootstep.getPlaceSound(), (Block.soundGlassFootstep.getVolume() + 1.0F) / 2.0F, Block.soundGlassFootstep.getPitch() * 0.8F);
				--stack.stackSize;

				tile.notifyExtendedNeighbours();

				return true;

			} else if (w.canPlaceEntityOnSide(this.itemID, x, y, z, false, side, ply, stack)) {
				if (w.isRemote)
					return true;

				Block var12 = Block.blocksList[this.itemID];
				int var13 = this.getMetadata(stack.getItemDamage());

				if (placeBlockAt(stack, ply, w, x, y, z, side, hitX, hitY, hitZ, var13)) {
					w.playSoundEffect(x + 0.5, y + 0.5, z + 0.5, Block.soundGlassFootstep.getPlaceSound(), (Block.soundGlassFootstep.getVolume() + 1.0F) / 2.0F, Block.soundGlassFootstep.getPitch() * 0.8F);
					--stack.stackSize;
				}

				return true;
			} else {
				return false;
			}
		}
	}

	@Override
	public boolean canPlaceItemBlockOnSide(World w, int x, int y, int z, int side, EntityPlayer ply, ItemStack stack) {
		int var11 = w.getBlockId(x, y, z);

		EnumWire type = WireDamageValues.getType(stack.getItemDamage());
		if (type == null)
			return false;

		if (!WireDamageValues.isJacketed(stack.getItemDamage())) {
			if (!BasicWireUtils.canPlaceWireOnSide(w, x, y, z, ForgeDirection.VALID_DIRECTIONS[side], false)) {
				return false;
			}

			boolean mergeIntoWireTile = false;
			boolean mergeIntoMicroblockTile = false;

			Block microblockContainerBlock = ProjectRed.blockMicrocontainer;

			if (var11 == Block.snow.blockID)
				side = 1;
			else if (!mergeIntoWireTile && !mergeIntoMicroblockTile && var11 != Block.vine.blockID && var11 != Block.tallGrass.blockID && var11 != Block.deadBush.blockID && (Block.blocksList[var11] == null || !Block.blocksList[var11].isBlockReplaceable(w, x, y, z))) {
				switch (side) {
				case Dir.NX:
					x--;
					break;
				case Dir.PX:
					x++;
					break;
				case Dir.NY:
					y--;
					break;
				case Dir.PY:
					y++;
					break;
				case Dir.NZ:
					z--;
					break;
				case Dir.PZ:
					z++;
					break;
				}
			}
			mergeIntoMicroblockTile = microblockContainerBlock != null && w.getBlockId(x, y, z) == microblockContainerBlock.blockID && !((IMicroblockSupporterTile) w.getBlockTileEntity(x, y, z)).getCoverSystem().isPositionOccupied(EnumPosition.getFacePosition(side ^ 1));
			mergeIntoWireTile = w.getBlockId(x, y, z) == ProjectRed.blockWire.blockID && ((TileWire) w.getBlockTileEntity(x, y, z)).canPlaceWireOnSide(type, side ^ 1);

			return mergeIntoMicroblockTile || mergeIntoWireTile || w.canPlaceEntityOnSide(this.itemID, x, y, z, false, side, ply, stack);

		} else {
			// jacketed wire
			if (var11 == ProjectRed.blockWire.blockID && ((TileWire) w.getBlockTileEntity(x, y, z)).canAddJacketedWire(type))
				return true;

			Block microblockContainerBlock = ProjectRed.blockMicrocontainer;

			if (var11 != Block.snow.blockID && var11 != Block.vine.blockID && var11 != Block.tallGrass.blockID && var11 != Block.deadBush.blockID && (Block.blocksList[var11] == null || !Block.blocksList[var11].isBlockReplaceable(w, x, y, z))) {
				switch (side) {
				case Dir.NX:
					x--;
					break;
				case Dir.PX:
					x++;
					break;
				case Dir.NY:
					y--;
					break;
				case Dir.PY:
					y++;
					break;
				case Dir.NZ:
					z--;
					break;
				case Dir.PZ:
					z++;
					break;
				}
			}

			boolean mergeIntoMicroblockTile = microblockContainerBlock != null && w.getBlockId(x, y, z) == microblockContainerBlock.blockID;
			if (mergeIntoMicroblockTile) {

				IMicroblockCoverSystem imcs = ((IMicroblockSupporterTile) w.getBlockTileEntity(x, y, z)).getCoverSystem();
				if (imcs.isPositionOccupied(EnumPosition.Centre) || imcs.isPositionOccupied(EnumPosition.PostX) || imcs.isPositionOccupied(EnumPosition.PostY) || imcs.isPositionOccupied(EnumPosition.PostZ))
					mergeIntoMicroblockTile = false;
			}

			return mergeIntoMicroblockTile || w.canPlaceEntityOnSide(this.itemID, x, y, z, false, side, ply, stack);
		}
	}

	@Override
	public boolean placeBlockAt(ItemStack stack, EntityPlayer player, World world, int x, int y, int z, int side, float hitX, float hitY, float hitZ, int metadata) {
		EnumWire type = WireDamageValues.getType(stack.getItemDamage());
		if (type == null)
			return false;

		if (!world.setBlock(x, y, z, itemID, EnumWire.CLASS_TO_META.get(type.teclass), 0))
			return false;

		if (world.getBlockId(x, y, z) == itemID) {
			Block.blocksList[itemID].onBlockPlacedBy(world, x, y, z, player, stack);

			TileWire tile;

			try {
				tile = type.teclass.getConstructor().newInstance();
			} catch (Exception e) {
				throw new RuntimeException(e);
			}

			if (WireDamageValues.isJacketed(stack.getItemDamage()))
				tile.rawAddJacketedWire(type);
			else
				tile.rawAddWire(type, side ^ 1);

			world.setBlockTileEntity(x, y, z, tile);
			tile.notifyExtendedNeighbours();
		}

		return true;
	}

	@Override
	public int getMetadata(int par1) {
		return par1;
	}
}
