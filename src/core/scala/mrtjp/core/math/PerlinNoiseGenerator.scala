/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.math

import java.util.Random

import net.minecraft.world.World

object PerlinNoiseGenerator extends PerlinNoiseGenerator
{
    protected def floor(x:Double) = if (x >= 0) x.toInt else x.toInt-1
    protected def fade(x:Double) = x*x*x*(x*(x*6-15)+10)
    protected def lerp(x:Double, y:Double, z:Double) = y+x*(z-y)
    protected def grad(hash1:Int, x:Double, y:Double, z:Double) =
    {
        val hash = hash1&15
        val u = if (hash < 8) x else y
        val v = if (hash < 4) y else if (hash == 12 || hash == 14) x else z
        (if ((hash&1) == 0) u else -u)+(if ((hash&2) == 0) v else -v)
    }
}

class PerlinNoiseGenerator(rand:Random)
{
    def this() = this(new Random)
    def this(w:World) = this(w.random)
    def this(seed:Long) = this(new Random(seed))

    private final val perm = new Array[Int](512)
    private val offsetX = rand.nextDouble*256
    private val offsetY = rand.nextDouble*256
    private val offsetZ = rand.nextDouble*256

    {
        for (i <- 0 until 256) perm(i) = rand.nextInt(256)

        for (i <- 0 until 256)
        {
            val pos = rand.nextInt(256-i)+i
            val old = perm(i)
            perm(i) = perm(pos)
            perm(pos) = old
            perm(i+256) = perm(i)

        }
    }

    def noise(x:Double):Double = noise(x, 0, 0)
    def noise(x:Double, y:Double):Double = noise(x, y, 0)

    /**
     * Computes and returns the 3D noise for the given coordinates in 3D space
     *
     * @param x X coordinate
     * @param y Y coordinate
     * @param z Z coordinate
     * @return Noise at given location, from range -1 to 1
     */
    def noise(x:Double, y:Double, z:Double):Double =
    {
        var x1 = x+offsetX
        var y1 = y+offsetY
        var z1 = z+offsetZ

        import PerlinNoiseGenerator._

        val floorX = floor(x1)
        val floorY = floor(y1)
        val floorZ = floor(z1)

        // Find unit cube containing the point
        val X = floorX&255
        val Y = floorY&255
        val Z = floorZ&255

        // Get relative xyz coordinates of the point within the cube
        x1 -= floorX
        y1 -= floorY
        z1 -= floorZ

        // Compute fade curves for xyz
        val fX = fade(x1)
        val fY = fade(y1)
        val fZ = fade(z1)

        // Hash coordinates of the cube corners
        val A = perm(X)+Y
        val AA = perm(A)+Z
        val AB = perm(A+1)+Z
        val B = perm(X+1)+Y
        val BA = perm(B)+Z
        val BB = perm(B+1)+Z

        lerp(fZ, lerp(fY, lerp(fX, grad(perm(AA), x1, y1, z1), grad(perm(BA), x1-1, y1, z1)), lerp(fX, grad(perm(AB),
            x1, y1-1, z1), grad(perm(BB), x1-1, y1-1, z1))), lerp(fY, lerp(fX, grad(perm(AA+1), x1, y1, z1-1),
            grad(perm(BA+1), x1-1, y1, z1-1)), lerp(fX, grad(perm(AB+1), x1, y1-1, z1-1),
            grad(perm(BB+1), x1-1, y1-1, z1-1))))
    }


    /**
     * Generates noise for the 3D coordinates using the specified number of
     * octaves and parameters
     *
     * @param x          X-coordinate
     * @param y          Y-coordinate
     * @param z          Z-coordinate
     * @param octaves    Number of octaves to use
     * @param frequency  How much to alter the frequency by each octave
     * @param amplitude  How much to alter the amplitude by each octave
     * @param normalized If true, normalize the value to [-1, 1]
     * @return Resulting noise
     */
    def noise(x:Double, y:Double, z:Double, octaves:Int, frequency:Double, amplitude:Double, normalized:Boolean):Double =
    {
        var result = 0.0D
        var amp = 1.0D
        var freq = 1.0D
        var max = 0.0D

        for (i <- 0 until octaves)
        {
            result += noise(x*freq, y*freq, z*freq)*amp
            max += amp
            freq *= frequency
            amp *= amplitude
        }

        if (normalized) result /= max
        result
    }
}