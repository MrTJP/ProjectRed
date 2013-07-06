package mrtjp.projectred.interfaces;


public interface IMultipartTile extends IPartContainer {
	/**
	 * Returns an ICoverSystem object, or null if this tile does not support a cover system
	 * @see mrtjp.projectred.interfaces.ICoverSystem 
	 */
	public ICoverSystem getCoverSystem();
}
