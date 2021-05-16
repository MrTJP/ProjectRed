/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.vec

import codechicken.lib.render.CCModel
import codechicken.lib.vec.Vector3
import net.minecraft.util.Direction
import net.minecraft.util.math.vector.Vector3d
import net.minecraft.util.math.{BlockPos, BlockRayTraceResult, RayTraceResult}

object ModelRayTracer
{
    def raytraceModel(x:Double, y:Double, z:Double, from1:Vector3d, to1:Vector3d, model:CCModel):RayTraceResult =
    {
        val from = new Vector3(from1)
        val to = new Vector3(to1)

        val offset = new Vector3(x, y, z)
        val start = from.copy.subtract(offset)
        val dir = to.copy.subtract(from)

        def getSide(vec:Vector3) =
        {
            import vec.{x => x1, y => y1, z => z1}
            Seq((-y1, 0), (y1, 1), (-z1, 2), (z1, 3), (-x1, 4), (x1, 5)).maxBy(_._1)._2
        }

        raytraceModel(start, dir, model) match
        {
            case Some((dist, tri)) =>
                val side = getSide(tri.normal.copy.add(start.copy.add(dir).multiply(dist).multiply(0.001)))
                val mop = new BlockRayTraceResult(calcPlayerHit(new Vector3(x, y, z), from.copy.add(dir.copy.multiply(dist))).vec3(),
                    Direction.values()(side), new BlockPos(x.toInt, y.toInt, z.toInt), false)
                mop.subHit = 0
                mop
            case None => null
        }
    }

    private def calcPlayerHit(b:Vector3, p:Vector3) =
    {
        val shift = 1/4096F
        val thresh = 0.5
        val c = p.copy.subtract(b).add(-0.5)
        val ac = new Vector3(c.x.abs, c.y.abs, c.z.abs)

        if (ac.x < thresh && ac.x >= ac.y && ac.x >= ac.z)
            new Vector3(if (c.x > 0) b.x + 1 - shift else b.x + shift, p.y, p.z)
        else if (ac.y < thresh && ac.y >= ac.z && ac.y >= ac.x)
            new Vector3(p.x, if (c.y > 0) b.y + 1 - shift else b.y + shift, p.z)
        else if (ac.z < thresh && ac.z >= ac.x && ac.z >= ac.y)
            new Vector3(p.x, p.y, if (c.z > 0) b.z + 1 - shift else b.z + shift)
        else p
    }

    private def raytraceModel(from:Vector3, dir:Vector3, model:CCModel) =
    {
        val faces = for (i <- model.getVertices.indices by 4) yield
            Quad(model.verts(i).vec, model.verts(i+1).vec, model.verts(i+2).vec, model.verts(i+3).vec)

        val tfaces = faces.flatMap(_.toTri)

        var currentHit:Option[(Double, Tri)] = None

        tfaces.foreach(t => mt(from, dir, t.v0, t.v1, t.v2) match
        {
            case Some(dist) =>
                if (currentHit.isEmpty || dist < currentHit.get._1)
                    currentHit = Option((dist, t))
            case None =>
        })
        currentHit
    }

    private def mt(origin:Vector3, dir:Vector3, v0:Vector3, v1:Vector3, v2:Vector3, cullBack:Boolean = true, epsilon:Double = 1e-6) =
    {
        // 2 edges of a triangle
        val e1 = v1.copy.subtract(v0)
        val e2 = v2.copy.subtract(v0)
        // determinant of the equation
        val p = dir.copy.crossProduct(e2)

        val det = e1.dotProduct(p)

        if (cullBack)
        {
            if (det < epsilon) None
            else
            {
                val t = origin.copy.subtract(v0)
                val du = t.dotProduct(p)
                if (du < 0.0 || du > det) None
                else
                {
                    val q = t.copy.crossProduct(e1)
                    val dv = dir.dotProduct(q)
                    if (dv < 0.0 || du+dv > det) None
                    else Some(e2.dotProduct(q)/det)
                }
            }
        }
        else
        {
            if (det < epsilon && det > -epsilon) None
            else
            {
                val invDet = 1.0/det
                val t = origin.copy.subtract(v0)
                val u = t.dotProduct(p)*invDet
                if (u < 0.0 || u > 1.0) None
                else
                {
                    val q = t.copy.crossProduct(e1)
                    val v = dir.dotProduct(q)*invDet
                    if (v < 0.0 || u+v > 1.0) None
                    else Some(e2.dotProduct(q)*invDet)
                }
            }
        }
    }

    private case class Tri(v0:Vector3, v1:Vector3, v2:Vector3)
    {
        val normal = v1.copy.subtract(v0).crossProduct(v2.copy.subtract(v0)).normalize()
    }

    private case class Quad(v0:Vector3, v1:Vector3, v2:Vector3, v3:Vector3)
    {
        def toTri = Seq(Tri(v0, v1, v2), Tri(v0, v2, v3))
    }
}
