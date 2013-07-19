package mrtjp.projectred.utils.codechicken.core.vec;

public class InvertX extends CoordinateSystem {
	@Override
	public void convert(Vector3 vec) {
		vec.x = (vec.x * -1);
	}
}
