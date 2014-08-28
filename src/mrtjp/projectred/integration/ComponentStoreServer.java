package mrtjp.projectred.integration;

import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.RedundantTransformation;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Scale;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;

public class ComponentStoreServer {
    public static IndexedCuboid6[] buildCubeArray(int xSize, int zSize, Cuboid6 box, Vector3 expand)
    {
        Vector3 min = box.min;
        Vector3 max = box.max;
        min.multiply(1/16D);
        max.multiply(1/16D);
        expand.multiply(1/16D);
        IndexedCuboid6[] data = new IndexedCuboid6[xSize*zSize];
        for (int i = 0; i < data.length; i++)
        {
            int x = i%xSize;
            int z = i/zSize;
            double dx = (max.x-min.x)/xSize;
            double dz = (max.z-min.z)/zSize;
            Vector3 min1 = new Vector3(min.x+dx*x, min.y, min.z+dz*z);
            Vector3 max1 = new Vector3(min1.x+dx, max.y, min1.z+dz);
            data[i] = new IndexedCuboid6(i, new Cuboid6(min1, max1).expand(expand));
        }
        return data;
    }

    public static Transformation orientT(int orient)
    {
        Transformation t = Rotation.sideOrientation(orient%24>>2, orient&3);
        if (orient >= 24)
            t = new Scale(-1, 1, 1).with(t);

        return t.at(Vector3.center);
    }

    public static Transformation dynamicT(int orient)
    {
        return orient == 0 ? new RedundantTransformation() : new Scale(-1, 1, 1).at(Vector3.center);
    }
}
