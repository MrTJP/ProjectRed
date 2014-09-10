package mrtjp.projectred.core.libmc

import codechicken.lib.raytracer.IndexedCuboid6
import codechicken.lib.vec._

object VecLib
{
    def buildCubeArray(xSize:Int, zSize:Int, box:Cuboid6, expand:Vector3):Array[IndexedCuboid6] =
    {
        import box.{min, max}
        min.multiply(1/16D)
        max.multiply(1/16D)
        expand.multiply(1/16D)
        val data = new Array[IndexedCuboid6](xSize * zSize)
        for (i <- 0 until data.length)
        {
            val x = i%xSize
            val z = i/zSize
            val dx = (max.x-min.x)/xSize
            val dz = (max.z-min.z)/zSize
            val min1 = new Vector3(min.x+dx*x, min.y, min.z+dz*z)
            val max1 = new Vector3(min1.x+dx, max.y, min1.z+dz)
            data(i) = new IndexedCuboid6(i, new Cuboid6(min1, max1).expand(expand))
        }
        data
    }

    def orientT(orient:Int) =
    {
        var t = Rotation.sideOrientation(orient%24>>2, orient&3)
        if (orient >= 24) t = new Scale(-1, 1, 1).`with`(t)
        t.at(Vector3.center)
    }
}
