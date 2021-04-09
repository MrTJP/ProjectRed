/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.vec

case class Vec3(dx:Double, dy:Double, dz:Double)
{
    override def equals(obj:scala.Any) = obj match
    {
        case that:Vec3 => dx == that.dx && dy == that.dy && dz == that.dz
        case _ => false
    }

    def copy = Vec3(dx, dy, dz)

    def add(dx0:Double, dy0:Double, dz0:Double) = Vec3(dx+dx0, dy+dy0, dz+dz0)
    def subtract(dx0:Double, dy0:Double, dz0:Double) = add(-dx0, -dy0, -dz0)
    def multiply(i:Double, j:Double, k:Double) = Vec3(dx*i, dy*j, dz*k)
    def divide(i:Double, j:Double, k:Double) = multiply(1/i, 1/j, 1/k)
    def cross(i:Double, j:Double, k:Double) = Vec3(dy*k-dz*j, dz*i-dx*k, dx*j-dy*i)
    def dot(i:Double, j:Double, k:Double) = dx*i+dy*j+dz*k

    def add(d:Double):Vec3 = add(d, d, d)
    def subtract(d:Double):Vec3 = subtract(d, d, d)
    def multiply(k:Double):Vec3 = multiply(k, k, k)
    def divide(k:Double):Vec3 = divide(k, k, k)
    def cross(k:Double):Vec3 = cross(k, k, k)
    def dot(k:Double):Double = dot(k, k, k)

    def add(that:Vec3):Vec3 = add(that.dx, that.dy, that.dz)
    def subtract(that:Vec3):Vec3 = subtract(that.dx, that.dy, that.dz)
    def multiply(that:Vec3):Vec3 = multiply(that.dx, that.dy, that.dz)
    def divide(that:Vec3):Vec3 = divide(that.dx, that.dy, that.dz)
    def cross(that:Vec3):Vec3 = cross(that.dx, that.dy, that.dz)
    def dot(that:Vec3):Double = dot(that.dx, that.dy, that.dz)

    def magSquared = dx*dx+dy*dy+dz*dz
    def mag = math.sqrt(magSquared)

    def normalize =
    {
        val l = mag
        if (l == 0) Vec3.zeroVec else this/l
    }

    def negate = Vec3(-dx, -dy, -dz)

    def scalarProject(that:Vec3):Double =
    {
        val l = that.mag
        if (l == 0) 0 else dot(that)/l
    }

    def project(that:Vec3):Vec3 =
    {
        val l = that.magSquared
        if (l == 0) return Vec3.zeroVec
        val m = dot(that)/l
        that*m
    }

    def reject(that:Vec3):Vec3 = this-project(that)

    def unary_- = negate
    def unary_~ = normalize

    def +(that:Vec3) = add(that)
    def -(that:Vec3) = subtract(that)
    def *(that:Vec3) = multiply(that)
    def /(that:Vec3) = divide(that)

    def +(that:Double) = add(that)
    def -(that:Double) = subtract(that)
    def *(that:Double) = multiply(that)
    def /(that:Double) = divide(that)

    override def toString = s"Vec3 @[$dx $dy $dz]"
}

object Vec3
{
    val zeroVec = Vec3(0, 0, 0)
    val oneVec = Vec3(1, 1, 1)
    val center = Vec3(0.5, 0.5, 0.5)
}