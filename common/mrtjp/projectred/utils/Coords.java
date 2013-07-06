package mrtjp.projectred.utils;

import java.util.ArrayList;
import java.util.List;

import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;

public class Coords {
	public int x;
	public int y;
	public int z;
	public ForgeDirection orientation;

	public Coords(int x, int y, int z) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.orientation = ForgeDirection.UNKNOWN;
	}

	public Coords(int x, int y, int z, ForgeDirection corientation) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.orientation = corientation;
	}

	public Coords(Coords p) {
		this.x = p.x;
		this.y = p.y;
		this.z = p.z;
		this.orientation = p.orientation;
	}

	public Coords(NBTTagCompound nbttagcompound) {
		this.x = nbttagcompound.getInteger("i");
		this.y = nbttagcompound.getInteger("j");
		this.z = nbttagcompound.getInteger("k");

		this.orientation = ForgeDirection.UNKNOWN;
	}

	public Coords(TileEntity tile) {
		this.x = tile.xCoord;
		this.y = tile.yCoord;
		this.z = tile.zCoord;
		this.orientation = ForgeDirection.UNKNOWN;
	}

	public Coords copy() {
		return new Coords(this.x, this.y, this.z, this.orientation);
	}

	public void moveRight(int step) {
		switch (orientation) {
		case SOUTH:
			this.x -= step;
			break;
		case NORTH:
			this.x += step;
			break;
		case EAST:
			this.z += step;
			break;
		case WEST:
			this.z -= step;
			break;
		default:
			break;
		}
	}

	public void moveLeft(int step) {
		moveRight(-step);
	}

	public void moveForwards(int step) {
		switch (orientation) {
		case UP:
			this.y += step;
			break;
		case DOWN:
			this.y -= step;
			break;
		case SOUTH:
			this.z += step;
			break;
		case NORTH:
			this.z -= step;
			break;
		case EAST:
			this.x += step;
			break;
		case WEST:
			this.x -= step;
			break;
		default:
			break;
		}
	}

	public void moveBackwards(int step) {
		moveForwards(-step);
	}

	public void moveUp(int step) {
		switch (orientation) {
		case SOUTH:
		case NORTH:
		case EAST:
		case WEST:
			this.y += step;
			break;
		default:
			break;
		}
	}

	public void moveDown(int step) {
		moveUp(-step);
	}

	public void writeToNBT(NBTTagCompound nbttagcompound) {
		nbttagcompound.setDouble("i", this.x);
		nbttagcompound.setDouble("j", this.y);
		nbttagcompound.setDouble("k", this.z);
	}

	public String toString() {
		if (this.orientation == null) {
			return "{" + this.x + ", " + this.y + ", " + this.z + "}";
		}
		return "{" + this.x + ", " + this.y + ", " + this.z + ";" + this.orientation.toString() + "}";
	}

	public boolean equals(Object obj) {
		if (!(obj instanceof Coords)) {
			return false;
		}
		Coords bp = (Coords) obj;
		return (bp.x == this.x) && (bp.y == this.y) && (bp.z == this.z) && (bp.orientation == this.orientation);
	}

	public int hashCode() {
		return this.x & 0xFFF | this.y & 0xFF00 | this.z & 0xFFF000;
	}

	public Coords min(Coords p) {
		return new Coords(p.x > this.x ? this.x : p.x, p.y > this.y ? this.y : p.y, p.z > this.z ? this.z : p.z);
	}

	public Coords max(Coords p) {
		return new Coords(p.x < this.x ? this.x : p.x, p.y < this.y ? this.y : p.y, p.z < this.z ? this.z : p.z);
	}

	public List<Coords> getAdjacent(boolean includeVertical) {
		List a = new ArrayList();
		a.add(new Coords(this.x + 1, this.y, this.z, ForgeDirection.EAST));
		a.add(new Coords(this.x - 1, this.y, this.z, ForgeDirection.WEST));
		a.add(new Coords(this.x, this.y, this.z + 1, ForgeDirection.SOUTH));
		a.add(new Coords(this.x, this.y, this.z - 1, ForgeDirection.NORTH));
		if (includeVertical) {
			a.add(new Coords(this.x, this.y + 1, this.z, ForgeDirection.UP));
			a.add(new Coords(this.x, this.y - 1, this.z, ForgeDirection.DOWN));
		}
		return a;
	}

	public TileEntity getTileEntity(World world, Class clazz) {
		return BasicUtils.getTileEntity(world, this, clazz);
	}

	public static TileEntity getAdjacentTileEntity(TileEntity start, ForgeDirection direction) {
		Coords p = new Coords(start);
		p.orientation = direction;
		p.moveForwards(1);
		return start.worldObj.getBlockTileEntity(p.x, p.y, p.z);
	}

	public static TileEntity getAdjacentTileEntity(TileEntity start, ForgeDirection direction, Class targetClass) {
		TileEntity te = getAdjacentTileEntity(start, direction);
		if (targetClass.isAssignableFrom(te.getClass())) {
			return te;
		}

		return null;
	}
}