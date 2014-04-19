package mrtjp.projectred.integration;

import codechicken.lib.vec.*;
import mrtjp.projectred.core.libmc.BasicUtils;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class ArrayCommons
{
    public static Cuboid6[][] oBoxes = new Cuboid6[6][2];
    public static Cuboid6[] cBoxes = new Cuboid6[6];

    static
    {
        oBoxes[0][0] = new Cuboid6(1/8D, 0, 0, 7/8D, 6/8D, 1);
        oBoxes[0][1] = new Cuboid6(0, 0, 1/8D, 1, 6/8D, 7/8D);
        cBoxes[0] = new Cuboid6(0, 0, 0, 1, 6/8D, 1);
        for (int s = 1; s < 6; s++)
        {
            Transformation t = Rotation.sideRotations[s].at(Vector3.center);
            oBoxes[s][0] = oBoxes[0][0].copy().apply(t);
            oBoxes[s][1] = oBoxes[0][1].copy().apply(t);
            cBoxes[s] = cBoxes[0].copy().apply(t);
        }
    }

    public static interface ITopArrayWire
    {
    }

    public static byte topWireConn(GatePart g)
    {
        byte i = 0;
        for (int r = 1; r < 4; r += 2)
            if (topWireConn(g, g.toAbsolute(r)))
                i |= 1 << r;

        return i;
    }

    private static boolean topWireConn(GatePart g, int r)
    {
        int absDir = Rotation.rotateSide(g.side(), r);

        BlockCoord pos = new BlockCoord(g.tile()).offset(absDir);
        TileMultipart t = BasicUtils.getMultipartTile(g.world(), pos);
        if (t != null)
        {
            TMultiPart tp = t.partMap(g.side());
            if (tp instanceof ITopArrayWire)
            {
                int a_r = ((GatePart) tp).rotation();
                return (a_r & 1) == (g.rotation() & 1);
            }
        }

        return false;
    }
}