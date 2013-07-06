package mrtjp.projectred.multipart.microblocks;

import mrtjp.projectred.multipart.IMultipartTile;

/**
 * A multipart tile that is compatible with microblocks.
 * Only tile entities should implement this.
 */
public interface IMicroblockSupporterTile extends IMultipartTile {
	/**
	 * Returns the microblock position a tile-owned part mainly occupies, or null if
	 * unknown or the part occupies the whole block.
	 * Used to determine the position of new microblocks placed by the player.
	 * 
	 * @param subHit The tile-owned part ID to check.
	 * @return The position the tile-owned part occupies.
	 */
	public EnumPosition getPartPosition(int subHit);
	
	/**
	 * Returns true if the tile is "in the way" of this part and will prevent it being placed.
	 * @param type The type of part being placed.
	 * @param pos The position the part is being placed in.
	 * @return False to prevent part placement, otherwise true.
	 */
	public boolean isPlacementBlockedByTile(PartType<?> type, EnumPosition pos);

	/**
	 * Returns true if the given position is occupied by the tile.
	 * Does not check for collisions with other positions.
	 */
	public boolean isPositionOccupiedByTile(EnumPosition pos);
	
	@Override
	public IMicroblockCoverSystem getCoverSystem();
}
