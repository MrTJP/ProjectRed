package mrtjp.projectred.utils;

public class BlockMetaPair {
	public int id, data;

	public BlockMetaPair(int blockID, int k) {
		this.id = blockID;
		this.data = k;
	}

	@Override
	public int hashCode() {
		return (data << 16) + id;
	}

	@Override
	public boolean equals(Object o) {
		try {
			BlockMetaPair bmp = (BlockMetaPair) o;
			return bmp.id == id && bmp.data == data;
		} catch (ClassCastException e) {
			return false;
		}
	}

	public static BlockMetaPair parse(String s) {
		String[] a = s.split(":");
		if (a.length != 2) {
			throw new NumberFormatException("Not a valid block ID/data value: " + s);
		}
		return new BlockMetaPair(Integer.parseInt(a[0]), Integer.parseInt(a[1]));
	}
}
