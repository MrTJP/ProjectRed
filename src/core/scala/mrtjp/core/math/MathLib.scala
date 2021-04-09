/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.math

import java.util.Random

import codechicken.lib.vec.Vector3
import net.minecraft.util.math.BlockPos

object MathLib
{
    def clamp(min:Float, max:Float, v:Float) = Math.min(max, Math.max(min, v))

    def normal(bc:BlockPos, dir:Int):(Int, Int) = normal(bc.getX, bc.getY, bc.getZ, dir)
    def normal(x:Int, y:Int, z:Int, dir:Int):(Int, Int) = dir match
    {
        case 0 => (x, z)
        case 1 => (x, z)
        case 2 => (x, y)
        case 3 => (x, y)
        case 4 => (y, z)
        case 5 => (y, z)
    }

    def basis(bc:BlockPos, dir:Int):Int = basis(bc.getX, bc.getY, bc.getZ, dir)
    def basis(x:Int, y:Int, z:Int, dir:Int):Int = dir match
    {
        case 0 => y
        case 1 => y
        case 2 => z
        case 3 => z
        case 4 => x
        case 5 => x
    }

    def shift(dir:Int) = if ((dir&1) == 1) 1 else -1

    def splitLine(xs:Seq[Int], shift:Int) =
    {
        if (xs.isEmpty) Seq()
        else
        {
            var start = 0
            val ret = for
            {
                (x, i) <- xs.zipWithIndex
                if i > 0
                if x != xs(i-1)+shift
            } yield
            {
                val size = i-start
                start = i
                (xs(i-1), size)
            }
            ret :+ ((xs.last, xs.length-start))
        }
    }

    def rhrAxis(dir:Int, normal:(Int, Int), basis:Int):BlockPos = dir match
    {
        case 0 => new BlockPos(normal._1, basis, normal._2)
        case 1 => new BlockPos(normal._1, basis, normal._2)
        case 2 => new BlockPos(normal._1, normal._2, basis)
        case 3 => new BlockPos(normal._1, normal._2, basis)
        case 4 => new BlockPos(basis, normal._1, normal._2)
        case 5 => new BlockPos(basis, normal._1, normal._2)
    }

    def bezier(s:Vector3, c1:Vector3, c2:Vector3, e:Vector3, t:Float):Vector3 =
    {
        if ((t < 0.0F) || (t > 1.0F)) return s
        val one_minus_t = 1.0F-t
        val retValue = new Vector3(0.0D, 0.0D, 0.0D)
        val terms = new Array[Vector3](4)

        def calcNewVector(scaler:Float, base:Vector3) = base.copy.multiply(scaler)

        terms(0) = calcNewVector(one_minus_t*one_minus_t*one_minus_t, s)
        terms(1) = calcNewVector(3.0F*one_minus_t*one_minus_t*t, c1)
        terms(2) = calcNewVector(3.0F*one_minus_t*t*t, c2)
        terms(3) = calcNewVector(t*t*t, e)

        for (i <- 0 until 4) retValue.add(terms(i))
        retValue
    }

    private val random = new Random
    def randomFromIntRange(az:Range, rand:Random = random) = az(rand.nextInt(az.size))

    def leastSignificant(mask:Int) =
    {
        var bit = 0
        var m = mask
        while ((m&1) == 0 && m != 0){ bit += 1; m <<= 1 }
        bit
    }

    def mostSignificant(mask:Int):Int =
    {
        if (mask == 0) return 0
        31-Integer.numberOfLeadingZeros(mask)
    }

    def weightedRandom[T](xs:Iterable[(T, Int)], rand:Random = random):T =
    {
        if (xs.size == 1) return xs.head._1
        var weight = rand.nextInt(xs.map(_._2).sum)
        xs.find(x => {weight -= x._2; weight < 0}).get._1
    }
}