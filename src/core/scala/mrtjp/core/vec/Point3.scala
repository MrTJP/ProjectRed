/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.vec

import net.minecraft.util.math.BlockPos

case class Point3(x:Int, y:Int, z:Int)
{
    override def equals(obj:scala.Any) = obj match
    {
        case Point3(x1, y1, z1) => x == x1 && y == y1 && z == z1
        case _ => false
    }

    def copy = Point3(x, y, z)

    def add(dx:Int, dy:Int, dz:Int) = Point3(x+dx, y+dy, z+dz)
    def subtract(dx:Int, dy:Int, dz:Int) = add(-dx, -dy, -dz)
    def multiply(i:Int, j:Int, k:Int) = Point3(x*i, y*j, z*k)
    def divide(i:Int, j:Int, k:Int) = Point3(x/i, y/j, z/k)

    def add(d:Int):Point3 = add(d, d, d)
    def subtract(d:Int):Point3 = subtract(d, d, d)
    def multiply(k:Int):Point3 = multiply(k, k, k)
    def divide(k:Int):Point3 = divide(k, k, k)

    def add(that:Point3):Point3 = add(that.x, that.y, that.z)
    def subtract(that:Point3):Point3 = subtract(that.x, that.y, that.z)
    def multiply(that:Point3):Point3 = multiply(that.x, that.y, that.z)
    def divide(that:Point3):Point3 = divide(that.x, that.y, that.z)

    def negate = Point3(-x, -y, -z)
    def vectorize = Vec3(x, y, z)

    def max(that:Point3) = Point3(x max that.x, y max that.y, z max that.z)
    def min(that:Point3) = Point3(x min that.x, y min that.y, z min that.z)

    def offset(dir:Int):Point3 = offset(dir, 1)
    def offset(dir:Int, amount:Int):Point3 = this+(Point3.dirOffsets(dir)*amount)

    def unary_- = negate

    def +(that:Point3) = add(that)
    def -(that:Point3) = subtract(that)
    def *(that:Point3) = multiply(that)
    def /(that:Point3) = divide(that)

    def +(that:Int) = add(that)
    def -(that:Int) = subtract(that)
    def *(that:Int) = multiply(that)
    def /(that:Int) = divide(that)

    override def toString = s"Point3 @[$x $y $z]"
}

object Point3
{
    val infinitePoint = Point3(Int.MaxValue, Int.MaxValue, Int.MaxValue)
    val zeroPoint = Point3(0, 0, 0)

    val dirOffsets = Seq(
        Point3( 0,-1, 0),
        Point3( 0, 1, 0),
        Point3( 0, 0,-1),
        Point3( 0, 0, 1),
        Point3(-1, 0, 0),
        Point3( 1, 0, 0)
    )

    def apply(vec3:Vec3):Point3 = Point3(vec3.dx.toInt, vec3.dy.toInt, vec3.dz.toInt)

    def apply(pos:BlockPos):Point3 = Point3(pos.getX, pos.getY, pos.getZ)
}
