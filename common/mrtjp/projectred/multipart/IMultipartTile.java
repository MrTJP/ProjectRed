package mrtjp.projectred.multipart;


public interface IMultipartTile extends IPartContainer {
	/**
	 * Returns an ICoverSystem object, or null if this tile does not support a cover system
	 * @see mrtjp.projectred.multipart.ICoverSystem 
	 */
	public ICoverSystem getCoverSystem();
}
