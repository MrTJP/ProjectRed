package mrtjp.projectred.lib;

import codechicken.lib.vec.*;


public class VecLib {

    public static Cuboid6[] buildCubeArray(int xSize, int zSize, Cuboid6 box, Vector3 expand) {

        box.min.multiply(1 / 16D);
        box.max.multiply(1 / 16D);
        expand.multiply(1 / 16D);

        Cuboid6[] cuboids = new Cuboid6[xSize * zSize];
        for (int i = 0; i < cuboids.length; i++) {

            int x = i % xSize;
            int z = i / zSize;

            double dx = (box.max.x - box.min.x) / xSize;
            double dz = (box.max.z - box.min.z) / zSize;
            Vector3 min1 = new Vector3(box.min.x + dx * x, box.min.y, box.min.z + dz * z);
            Vector3 max1 = new Vector3(min1.x + dx, box.max.y, min1.z + dz);
            cuboids[i] = new Cuboid6(min1, max1).expand(expand);
        }
        return cuboids;
    }

    public static Transformation orientT(int orient) {
        Transformation t = Rotation.sideOrientation(orient % 24 >> 2, orient & 3);
        if (orient >= 24) t = new Scale(-1, 1, 1).with(t);
        return t.at(Vector3.CENTER);
    }
}
