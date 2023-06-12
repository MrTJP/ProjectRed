package mrtjp.projectred.lib;

import codechicken.lib.render.CCModel;
import codechicken.lib.util.VectorUtils;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.Vertex5;
import com.mojang.blaze3d.vertex.VertexFormat;
import it.unimi.dsi.fastutil.doubles.DoubleList;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.VoxelShape;
import org.jetbrains.annotations.Nullable;

import java.util.LinkedList;
import java.util.List;

/**
 * Shape that can calculate a hit against arbitrary triangles
 */
public class ModelVoxelShape extends VoxelShape {

    private final VoxelShape parent;
    private final List<Tri> tris;

    /**
     * Creates a VoxelShape with properties of the provided parent, but that traces
     * against only against the provided triangles.
     *
     * @param parent The parent shape for render outline. Typically, something similar to the actual model.
     * @param tris   Triangles for ray tracing
     */
    public ModelVoxelShape(VoxelShape parent, List<Tri> tris) {
        super(parent.shape);
        this.parent = parent;
        this.tris = tris;
    }

    public ModelVoxelShape(VoxelShape parent, CCModel model) {
        this(parent, trisFromCCModel(model));
    }

    public ModelVoxelShape(VoxelShape parent, Vertex5[] verts, VertexFormat.Mode vertexMode) {
        this(parent, trisFromVerts(verts, vertexMode));
    }

    @Override
    public DoubleList getCoords(Direction.Axis axis) {
        return parent.getCoords(axis);
    }

    @Nullable
    @Override
    public BlockHitResult clip(Vec3 start, Vec3 end, BlockPos pos) {

        Vector3 relativeStart = new Vector3(start).subtract(pos.getX(), pos.getY(), pos.getZ());
        Vector3 relativeEnd = new Vector3(end).subtract(pos.getX(), pos.getY(), pos.getZ());

        Vector3 intersection = null;
        Direction side = null;
        double dist = Double.MAX_VALUE;

        List<Tri> hits = new LinkedList<>();

        for (Tri tri : tris) {
            Vector3 i = tri.getIntersection(relativeStart, relativeEnd, true, false);
            if (i == null) continue;

            hits.add(tri);
            double d = relativeStart.copy().subtract(i).magSquared();

            if (dist > d) {
                intersection = i;
                side = tri.side;
                dist = d;
            }
        }

        if (intersection == null) return null;

        return new BlockHitResult(intersection.add(pos).vec3(), side, pos, true);
    }

    public static List<Tri> trisFromCCModel(CCModel model) {
        return trisFromVerts(model.verts, model.vertexMode);
    }

    public static List<Tri> trisFromVerts(Vertex5[] verts, VertexFormat.Mode vertexMode) {

        int vp = vertexMode == VertexFormat.Mode.QUADS ? 4 : 3;

        List<Tri> triList = new LinkedList<>();
        for (int i = 0; i < verts.length; i += vp) {
            if (vertexMode == VertexFormat.Mode.QUADS) {
                triList.add(new Tri(
                        verts[i    ].copy(),
                        verts[i + 1].copy(),
                        verts[i + 2].copy()));
                triList.add(new Tri(
                        verts[i    ].copy(),
                        verts[i + 2].copy(),
                        verts[i + 3].copy()));
            } else {
                triList.add(new Tri(
                        verts[i    ].copy(),
                        verts[i + 1].copy(),
                        verts[i + 2].copy()));
            }
        }

        return triList;
    }

    public static class Tri {

        public final Vector3 v0;
        public final Vector3 v1;
        public final Vector3 v2;
        public final Vector3 normal;
        public final Direction side;

        // Precalculated constants for tracing
        private final Vector3 u;
        private final Vector3 v;
        private final double uu;
        private final double uv;
        private final double vv;
        private final double D;

        public Tri(Vector3 v0, Vector3 v1, Vector3 v2) {
            this.v0 = v0;
            this.v1 = v1;
            this.v2 = v2;
            this.normal = VectorUtils.calculateNormal(v0, v1, v2);
            this.side = VectorUtils.calcNormalSide(normal);

            // Precompute some constants to speed up tracing
            u = v1.copy().subtract(v0);
            v = v2.copy().subtract(v0);
            uu = u.dotProduct(u);
            uv = u.dotProduct(v);
            vv = v.dotProduct(v);
            D = uv * uv - uu * vv;
        }

        public Tri(Vertex5 v0, Vertex5 v1, Vertex5 v2) {
            this(v0.vec, v1.vec, v2.vec);
        }

        /**
         * Fast implementation of ray-triangle intersection using barycentric coordinates. Because
         * normal and uu/uv/vv are precomputed, this requires only 2 dot products to check for
         * plane intersection and 2 more to check for triangle intersection.
         * <p>
         * Source: Daniel Sunday <a href="http://www.geomalgorithms.com/algorithms.html">...</a>
         *
         * @param r0      First ray point
         * @param r1      Second ray point
         * @param cull    If true, only intersections on triangle's front face are valid
         * @param segment If true, ray is treated as a segment (has a start and end). Otherwise, it is treated as infinite length
         * @return Intersection point or null if no intersection
         */
        public Vector3 getIntersection(Vector3 r0, Vector3 r1, boolean cull, boolean segment) {

            Vector3 dir = r1.copy().subtract(r0);
            Vector3 w0 = r0.copy().subtract(v0);
            double a = -normal.dotProduct(w0);
            double b = normal.dotProduct(dir);

            // When b is zero, ray is parallel to triangle plane (within floating point error)
            if (Math.abs(b) < 0.00000001) return null;

            // When b is negative, ray points against the normal towards the front face. If culling is enabled, b must be negative
            if (cull && (b > 0)) return null;

            // Find line scalar r. This represents how from r0 towards r1 the intersection is:
            // - When r == 0, intersection is at r0
            // - When r == 1, intersection is at r1
            // - When r is negative, intersection is behind r0 (the wrong direction)
            // - When r is greater than 1, intersection is past r1 (okay only if ray is infinite and not a segment)
            double r = a / b;
            if (r < 0.0 || (segment && r > 1.0)) return null;

            // Intersection of ray against the triangle plane
            Vector3 intersection = r0.copy().add(dir.x * r, dir.y * r, dir.z * r);

            // Now check if I is inside the triangle
            Vector3 w = intersection.copy().subtract(v0);
            double wu = w.dotProduct(u);
            double wv = w.dotProduct(v);

            double s = (uv * wv - vv * wu) / D;
            if (s < 0.0 || s > 1.0) return null; // I is outside the triangle

            double t = (uv * wu - uu * wv) / D;
            if (t < 0.0 || (s + t) > 1.0) return null; // I is outside the triangle

            return intersection;
        }
    }
}
