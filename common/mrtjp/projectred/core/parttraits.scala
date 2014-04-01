package mrtjp.projectred.core

import codechicken.lib.data.{MCDataOutput, MCDataInput}
import codechicken.lib.vec.{Vector3, Rotation}
import codechicken.multipart.{TSlottedPart, TFacePart, TMultiPart}

trait TSwitchPacket extends TMultiPart
{
    override final def read(packet:MCDataInput)
    {
        read(packet, packet.readUByte())
    }

    def read(packet:MCDataInput, key:Int)

    def getWriteStreamOf(key:Int):MCDataOutput = getWriteStream.writeByte(key)
}

trait TFaceOrient extends TMultiPart with TFacePart
{
    var orientation:Byte = 0

    def side = orientation>>2

    def setSide(s:Int)
    {
        val oldOrient = orientation
        orientation = (orientation&0x3|s<<2).asInstanceOf[Byte]
        if (oldOrient != orientation) onOrientationChanged(oldOrient)
    }

    def rotation = orientation&0x3

    def setRotation(r:Int)
    {
        val oldOrient = orientation
        orientation = (orientation&0xFC|r).asInstanceOf[Byte]
        if (oldOrient != orientation) onOrientationChanged(oldOrient)
    }

    def rotationT = Rotation.sideOrientation(side, rotation).at(Vector3.center)

    def onOrientationChanged(oldOrient:Int) {}

    // internal r from external absRot
    def toInternal(absRot:Int) = (absRot+6-rotation)%4
    // absRot from internal r
    def toAbsolute(r:Int) = (r+rotation+2)%4

    // absDir from absRot
    def absoluteDir(absRot:Int) = Rotation.rotateSide(side, absRot)

    // internal r from absDir
    def relRot(absDir:Int) = toInternal(Rotation.rotationTo(side, absDir))

    override def getSlotMask = 1<<side
}

trait TCenterOrient extends TMultiPart with TSlottedPart
{
    override def getSlotMask = 1<<6
}