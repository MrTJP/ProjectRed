package mrtjp.projectred.transmission

import codechicken.multipart.TMultiPart
import net.minecraft.nbt.NBTTagCompound

trait TBundledAquisitionsCommons extends TAcquisitionsCommons
{
    def calcStraightArray(dir:Int):Array[Byte]

    def calcInternalArray(dir:Int):Array[Byte]

    def resolveArray(part:TMultiPart, r:Int):Array[Byte]
}

trait TFaceBundledAquisitions extends TBundledAquisitionsCommons with TFaceAcquisitions
{
    def calcCornerArray(r:Int) = resolveArray(getCorner(r), rotFromCorner(r))

    def calcStraightArray(r:Int) = resolveArray(getStraight(r), rotFromStraight(r))

    def calcInternalArray(r:Int) = resolveArray(getInternal(r), rotFromInternal(r))

    def calcCenterArray = resolveArray(getCenter, side)
}

trait TCenterBundledAquisitions extends TBundledAquisitionsCommons with TCenterAcquisitions
{
    def calcStraightArray(s:Int) = resolveArray(getStraight(s), s^1)

    def calcInternalArray(s:Int) = resolveArray(getInternal(s), s^1)
}

object BundledCommons
{
    def signalsEqual(signal1:Array[Byte], signal2:Array[Byte]):Boolean =
    {
        if (signal1 == null) return isSignalZero(signal2)
        if (signal2 == null) return isSignalZero(signal1)
        signal1.sameElements(signal2)
    }

    def isSignalZero(signal:Array[Byte]):Boolean =
    {
        if (signal == null) return true
        for (i <- 0 until 16) if (signal(i) != 0) return false
        true
    }

    def isSignalZero(signal:Array[Byte], mask:Int):Boolean =
    {
        if (signal == null) return true
        for (i <- 0 until 16) if ((mask&1<<i) != 0 && signal(i) != 0) return false
        true
    }

    def dropSignalsLessThan(inThis:Array[Byte], fromThat:Array[Byte]) =
    {
        var dropped = false
        for (i <- 0 until 16) if ((fromThat(i)&0xFF) < (inThis(i)&0xFF))
        {
            inThis(i) = 0.asInstanceOf[Byte]
            dropped = true
        }
        dropped
    }

    def applyChangeMask(from:Array[Byte], to:Array[Byte], mask:Int)
    {
        for (i <- 0 until 16) if ((mask&1<<i) == 0) to(i) = from(i)
    }

    def raiseSignal(ofThis:Array[Byte], fromThat:Array[Byte]):Array[Byte] =
    {
        val sig1 = if (ofThis == null) new Array[Byte](16) else ofThis
        if (fromThat == null) return ofThis
        for (i <- 0 until 16) if ((sig1(i)&0xFF) < (fromThat(i)&0xFF)) sig1(i) = fromThat(i)
        sig1
    }

    def copySignal(signal:Array[Byte]) = if (signal == null) null else signal.clone()

    def saveSignal(tag:NBTTagCompound, key:String, signal:Array[Byte])
    {
        if (signal != null) tag.setByteArray(key, signal)
    }

    def loadSignal(tag:NBTTagCompound, key:String) =
    {
        if (tag.hasKey(key)) tag.getByteArray(key) else null
    }

    def packDigital(signal:Array[Byte]):Int =
    {
        if (signal == null) return 0
        var packed = 0
        for (i <- 0 until 16) if (signal(i) != 0) packed |= 1<<i
        packed
    }

    def unpackDigital(signal:Array[Byte], packed:Int):Array[Byte] =
    {
        if (packed == 0) return null
        val sig = if (signal == null) new Array[Byte](16) else signal
        for (i <- 0 until 16) sig(i) = (if ((packed&1<<i) == 0) 0 else 255).asInstanceOf[Byte]
        sig
    }
}