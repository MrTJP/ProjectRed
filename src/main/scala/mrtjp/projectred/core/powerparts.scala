/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.core

import codechicken.multipart.TMultiPart

import scala.ref.WeakReference

trait TCachedPowerConductor extends IPowerConnectable
{
    var needsCache = true
    var condCache = new Array[WeakReference[PowerConductor]](idRange.max+1)

    def idRange:Seq[Int]

    def getExternalCond(id:Int):PowerConductor

    override def conductorOut(id:Int) =
    {
        if (needsCache) rebuildCache()

        var wr = condCache(id)

        wr match {
            case WeakReference(cond) if cond.isValid =>
            case _ =>
                rebuildCache()
                wr = condCache(id)
        }

        wr match {
            case WeakReference(cond) if cond.isValid => cond
            case _ => null
        }
    }

    def rebuildCache()
    {
        for (i <- 0 until condCache.length) condCache(i) = new WeakReference(null)
        for (id <- idRange) {
            val c = getExternalCond(id)
            if (c != null && c.isValid) condCache(id) = new WeakReference(c)
        }
        needsCache = false
    }
}

trait TPowerPartCommons extends TMultiPart with TCachedPowerConductor with TConnectableCommons
{
    abstract override def onMaskChanged()
    {
        super.onMaskChanged()
        needsCache = true
    }
}

trait TFacePowerPart extends TMultiPart with TFaceConnectable with TPowerPartCommons
{
    override def idRange = 0 until 5

    override def getExternalCond(id:Int):PowerConductor =
    {
        if (0 to 3 contains id)
        {
            if (!maskConnects(id)) return null
            if ((connMap&1<<id) != 0) getCorner(id) match //corner
            {
                case p:IPowerConnectable => return p.conductor(rotFromCorner(id))
                case _ => world.getTileEntity(posOfCorner(id)) match
                {
                    case p:IPowerConnectable if outsideCornerEdgeOpen(id) => return p.conductor(absoluteDir(rotFromCorner(id)))
                    case _ =>
                }
            }
            else if ((connMap&0x10<<id) != 0) getStraight(id) match //straight
            {
                case p:IPowerConnectable => return p.conductor(rotFromStraight(id))
                case _ => world.getTileEntity(posOfStraight(id)) match
                {
                    case p:IPowerConnectable => return p.conductor(absoluteDir(rotFromStraight(id)))
                    case _ =>
                }
            }
            else if ((connMap&0x100<<id) != 0) getInternal(id) match //internal face
            {
                case p:IPowerConnectable => return p.conductor(id)
                case _ =>
            }
        }
        else if (id == 4) getCenter match
        {
            case p:IPowerConnectable => return p.conductor(side)
            case _ =>
        }

        null
    }
}

trait TCenterPowerPart extends TMultiPart with TCenterConnectable with TPowerPartCommons
{
    override def idRange = 0 until 6

    override def getExternalCond(id:Int):PowerConductor =
    {
        if (0 until 6 contains id)
        {
            if ((connMap&1<<id) != 0) getStraight(id) match //straight
            {
                case p:IPowerConnectable => return p.conductor(id^1)
                case _ => world.getTileEntity(posOfStraight(id)) match {
                    case p:IPowerConnectable => return p.conductor(id^1)
                    case _ =>
                }
            }
            else if ((connMap&1<<id+6) != 0) getInternal(id) match //internal
            {
                case p:IPowerConnectable => return p.conductor(id^1)
                case _ =>
            }
        }
        null
    }
}