package mrtjp.projectred.integration;

import mrtjp.projectred.core.BasicUtils;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class ArrayCommons
{
    public static interface ITopArrayWire
    {
    }

    public static byte topWireConn(GatePart g) {
        byte i = 0;
        for(int r = 1; r < 4; r+=2)
            if(topWireConn(g, g.toAbsolute(r)))
                i|=1<<r;
        
        return i;
    }
    
    private static boolean topWireConn(GatePart g, int r) {
        int absDir = Rotation.rotateSide(g.side(), r);
        
        BlockCoord pos = new BlockCoord(g.tile()).offset(absDir);
        TileMultipart t = BasicUtils.getMultipartTile(g.world(), pos);
        if (t != null) {
            TMultiPart tp = t.partMap(g.side());
            if(tp instanceof ITopArrayWire) {
                int a_r = ((GatePart) tp).rotation();
                return (a_r&1) == (g.rotation()&1);
            }
        }
        
        return false;
    }
}