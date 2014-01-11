package mrtjp.projectred.api;

/**
 * Marker interface for a screwdriver. Things like gates check if the item used
 * to right-click is an instance of this.
 */
public interface IScrewdriver
{
	public void damageScrewdriver(World world, EntityPlayer player); // Damage the item on usage
}
