package mrtjp.projectred.core

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.{Rotation, Vector3}
import codechicken.multipart.api.part.{TFacePart, TMultiPart, TSlottedPart}

import java.util.function.Consumer

trait TSwitchPacket extends TMultiPart
{
    override final def readUpdate(packet:MCDataInput)
    {
        read(packet, packet.readUByte())
    }

    def read(packet:MCDataInput, key:Int) = key match
    {
        case 0 => readDesc(packet)
        case _ =>
    }

    override final def sendUpdate(func: Consumer[MCDataOutput]) {
        sendUpdate(0, func.accept)
    }

    def sendUpdate(key:Int, func: MCDataOutput => Unit = _ => {}) =
        super.sendUpdate(p => {
            p.writeByte(key)
            func(p)
        })
}

trait TFaceOrient extends TMultiPart with TFacePart
{
    var orientation:Byte = 0

    def side = orientation>>2

    def setSide(s:Int)
    {
        val oldOrient = orientation
        orientation = (orientation&0x3|s<<2).toByte
        if (oldOrient != orientation) onOrientationChanged(oldOrient)
    }

    def rotation = orientation&0x3

    def setRotation(r:Int)
    {
        val oldOrient = orientation
        orientation = (orientation&0xFC|r).toByte
        if (oldOrient != orientation) onOrientationChanged(oldOrient)
    }

    def rotationT = Rotation.sideOrientation(side, rotation).at(Vector3.CENTER)

    def onOrientationChanged(oldOrient:Int) {}

    // internal r from absRot
    def toInternal(absRot:Int) = (absRot+6-rotation)%4
    // absRot from internal r
    def toAbsolute(r:Int) = (r+rotation+2)%4

    // absDir from absRot
    def absoluteDir(absRot:Int) = Rotation.rotateSide(side, absRot)
    // absRot from absDir
    def absoluteRot(absDir:Int) = Rotation.rotationTo(side, absDir)

    def toInternalMask(mask:Int) = TFaceOrient.shiftMask(mask, toInternal(0))
    def toAbsoluteMask(mask:Int) = TFaceOrient.shiftMask(mask, toAbsolute(0))

    override def getSlotMask = 1<<side
}

object TFaceOrient
{
    def shiftMask(mask:Int, r:Int) = (mask& ~0xF)|(mask<<r|mask>>4-r)&0xF
    def flipMaskZ(mask:Int) = mask&5|mask<<2&8|mask>>2&2
}

trait TCenterOrient extends TMultiPart with TSlottedPart
{
    override def getSlotMask = 1<<6
}
