package mrtjp.projectred.core

import codechicken.lib.vec.{Rotation, Vector3}
import net.minecraft.tileentity.TileEntity

trait TTileOrient extends TileEntity
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

    // internal r from absRot
    def toInternal(absRot:Int) = (absRot+6-rotation)%4
    // absRot from internal r
    def toAbsolute(r:Int) = (r+rotation+2)%4

    // absDir from absRot
    def absoluteDir(absRot:Int) = Rotation.rotateSide(side, absRot)
    // absRot from absDir
    def absoluteRot(absDir:Int) = Rotation.rotationTo(side, absDir)
}