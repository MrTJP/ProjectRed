package mrtjp.projectred.core.utils;

import java.util.Map;

import net.minecraft.util.Vec3;
import net.minecraftforge.common.ForgeDirection;

import org.lwjgl.input.Mouse;

import com.google.common.collect.Maps;

public class DirectionalRayTracer
{

    public static class HitCoord
    {
        public final ForgeDirection side;
        public final Vec3 coord;

        public HitCoord(ForgeDirection side, Vec3 coord) {
            this.side = side;
            this.coord = coord;
        }
    }

    private final double negX, negY, negZ;
    private final double posX, posY, posZ;

    public DirectionalRayTracer(double negX, double negY, double negZ, double posX, double posY, double posZ) {
        this.negX = negX;
        this.negY = negY;
        this.negZ = negZ;
        this.posX = posX;
        this.posY = posY;
        this.posZ = posZ;
    }

    public DirectionalRayTracer(double halfSize) {
        negX = negY = negZ = -halfSize;
        posX = posY = posZ = +halfSize;
    }

    private static Vec3 getMouseVector(float z) {
        return OpenGLLib.unproject(Mouse.getX(), Mouse.getY(), z);
    }

    private Vec3 calculateXPoint(Vec3 near, Vec3 diff, double x) {
        double p = (x - near.xCoord) / diff.xCoord;

        double y = near.yCoord + diff.yCoord * p;
        double z = near.zCoord + diff.zCoord * p;

        if (negY <= y && y <= posY && negZ <= z && z <= posZ)
            return Vec3.createVectorHelper(x, y, z);

        return null;
    }

    private Vec3 calculateYPoint(Vec3 near, Vec3 diff, double y) {
        double p = (y - near.yCoord) / diff.yCoord;

        double x = near.xCoord + diff.xCoord * p;
        double z = near.zCoord + diff.zCoord * p;

        if (negX <= x && x <= posX && negZ <= z && z <= posZ)
            return Vec3.createVectorHelper(x, y, z);

        return null;
    }

    private Vec3 calculateZPoint(Vec3 near, Vec3 diff, double z) {
        double p = (z - near.zCoord) / diff.zCoord;

        double x = near.xCoord + diff.xCoord * p;
        double y = near.yCoord + diff.yCoord * p;

        if (negX <= x && x <= posX && negY <= y && y <= posY)
            return Vec3.createVectorHelper(x, y, z);

        return null;
    }

    private static void addPoint(Map<ForgeDirection, Vec3> map, ForgeDirection side, Vec3 value) {
        if (value != null)
            map.put(side, value);
    }

    private Map<ForgeDirection, Vec3> calculateHitPoints(Vec3 near, Vec3 far) {
        Vec3 diff = far.subtract(near);

        Map<ForgeDirection, Vec3> result = Maps.newEnumMap(ForgeDirection.class);
        addPoint(result, ForgeDirection.WEST, calculateXPoint(near, diff, negX));
        addPoint(result, ForgeDirection.EAST, calculateXPoint(near, diff, posX));

        addPoint(result, ForgeDirection.DOWN, calculateYPoint(near, diff, negY));
        addPoint(result, ForgeDirection.UP, calculateYPoint(near, diff, posY));

        addPoint(result, ForgeDirection.NORTH, calculateZPoint(near, diff, negZ));
        addPoint(result, ForgeDirection.SOUTH, calculateZPoint(near, diff, posZ));
        return result;
    }

    public Map<ForgeDirection, Vec3> calculateMouseHits() {
        OpenGLLib.updateMatrices();
        Vec3 near = getMouseVector(0);
        Vec3 far = getMouseVector(1);

        return calculateHitPoints(near, far);
    }

    public HitCoord getNearestHit() {
        OpenGLLib.updateMatrices();
        Vec3 near = getMouseVector(0);
        Vec3 far = getMouseVector(1);

        Map<ForgeDirection, Vec3> hits = calculateHitPoints(near, far);

        if (hits.isEmpty())
            return null;

        ForgeDirection minForgeDirection = null;
        double minDist = Double.MAX_VALUE;

        for (Map.Entry<ForgeDirection, Vec3> e : hits.entrySet()) {
            double dist = e.getValue().subtract(near).lengthVector();
            if (dist < minDist) {
                minDist = dist;
                minForgeDirection = e.getKey();
            }
        }

        if (minForgeDirection == null)
            return null;

        return new HitCoord(minForgeDirection, hits.get(minForgeDirection));
    }
}
