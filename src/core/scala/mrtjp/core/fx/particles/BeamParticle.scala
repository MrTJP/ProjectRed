///*
// * Copyright (c) 2015.
// * Created by MrTJP.
// * All rights reserved.
// */
//package mrtjp.core.fx.particles
//
//import net.minecraft.client.Minecraft
//import net.minecraft.client.particle.EntityFX
//import net.minecraft.client.renderer.Tessellator
//import net.minecraft.world.World
//import org.lwjgl.opengl.GL11
//
//class BeamParticle(w:World) extends CoreParticle(w)
//{
//    var dX = 0.0
//    var dY = 0.0
//    var dZ = 0.0
////    var updateX = 0.0
////    var updateY = 0.0
////    var updateZ = 0.0
//
//    var rotateSpeed = 30.0
//    var maxLengthAge = 10
//    var fppc = false
//
//    private var length = 0.0
//    private var positionChanged = false
//
//    private def calculateLengthAndRotation()
//    {
//        val deltaX = posX-dX
//        val deltaY = posY-dY
//        val deltaZ = posZ-dZ
//
//        val hDist = math.sqrt(deltaX*deltaX+deltaZ*deltaZ)
//        length = math.sqrt(deltaX*deltaX+deltaY*deltaY+deltaZ*deltaZ)
//        rotationYaw = (math.atan2(deltaX, deltaZ)*180.0D/3.141592653589793D).toFloat
//        rotationPitch = (math.atan2(deltaY, hDist)*180.0D/3.141592653589793D).toFloat
//    }
//
////    def setBeamLocationAndTarget(double posX, double posY, double posZ, double targetX, double targetY, double targetZ)
////    {
////        updateX = posX
////        updateY = posY
////        updateZ = posZ
////        dX = targetX
////        dY = targetY
////        dZ = targetZ
////
////        if (particleAge > particleMaxAge - 5) {
////            particleMaxAge = (particleAge + 5)
////        }
////
////        positionChanged = true
////    }
//
//    def updatePositions()
//    {
//        if (posX != prevPosX || posY != prevPosY || posZ != prevPosZ)
//        {
//            if (fppc)
//            {
//                val player = Minecraft.getMinecraft.thePlayer
//                if (player != null)
//                {
//                    val yaw = player.rotationYaw
//                    val rotationYaw = yaw*3.141592653589793D/180.0D
//                    var offsetX = math.cos(rotationYaw) * 0.06F
//                    var offsetZ = math.sin(rotationYaw) * 0.06F
//                    posX -= offsetX
//                    posZ -= offsetZ
//                    posY += 0.05999999865889549D
//                    prevPosX = posX
//                    prevPosY = posY
//                    prevPosZ = posZ
//                }
//            }
//        }
//    }
//
//    override def onUpdate()
//    {
//        super.onUpdate()
//        updatePositions()
//        calculateLengthAndRotation()
//    }
//
//    override def renderParticle(tess:Tessellator, par2:Float, par3:Float, par4:Float, par5:Float, par6:Float, par7:Float)
//    {
//        super.renderParticle(tess, par2, par3, par4, par5, par6, par7)
//
//        tess.draw()
//
//        GL11.glPushMatrix()
//
//        val scaleFactor = 1.0
//        val slide = worldObj.getTotalWorldTime
//        val rot = worldObj.provider.getWorldTime%(360.0/rotateSpeed)*rotateSpeed+rotateSpeed*par2
//
//        val size = math.min(particleAge/maxLengthAge, 1.0)
//
//        val op = 0.4
//        val widthMod = if (fppc) 0.3 else 1.0
//
//        GL11.glTexParameterf(3553, 10242, 10497.0F)
//        GL11.glTexParameterf(3553, 10243, 10497.0F)
//
//        val var11 = slide+par2
//        val var12 = -var11*0.2F-math.floor(-var11*0.1F)
//
//        val xx = prevPosX+(posX-prevPosX)*par2-EntityFX.interpPosX
//        val yy = prevPosY+(posY-prevPosY)*par2-EntityFX.interpPosY
//        val zz = prevPosZ+(posZ-prevPosZ)*par2-EntityFX.interpPosZ
//        GL11.glTranslated(xx, yy, zz)
//
//        val deltaYaw = Math.abs(rotationYaw)-Math.abs(prevRotationYaw)
//
//        val ry = prevRotationYaw+deltaYaw*par2
//        val rp = prevRotationPitch+(rotationPitch-prevRotationPitch)*par2
//        GL11.glRotatef(90.0F, 1.0F, 0.0F, 0.0F)
//        GL11.glRotatef(180.0F+ry, 0.0F, 0.0F, -1.0F)
//        GL11.glRotatef(rp, 1.0F, 0.0F, 0.0F)
//
//        val offset1 = -0.15D*widthMod*size
//        val offset2 = 0.15D*widthMod*size
//        val offset3 = -0.15D*widthMod*size*1.0D
//        val offset4 = 0.15D*widthMod*size*1.0D
//
//        GL11.glRotatef(rot.toFloat, 0.0F, 1.0F, 0.0F)
//        val i = 5
////        if (AMCore.config.LowGFX()) {
////            i = 3
////            inc = 90.0F
////        } else if (AMCore.config.NoGFX()) {
////            i = 1
////            inc = 180.0F
////        }
//        for (t <- 0 until i)
//        {
//            val l = length*size*scaleFactor
//            val tl = particleIcon.getMinU
//            val br = particleIcon.getMaxU
//            val mU = particleIcon.getMinV
//            val mV = particleIcon.getMaxV
//
//            GL11.glRotatef(36.0F, 0.0F, 1.0F, 0.0F)
//            tess.startDrawingQuads()
//            tess.setBrightness(200)
//            if (t%2 == 0)
//                tess.setColorRGBA_F(particleRed, particleGreen, particleBlue, op.toFloat)
//            else {
//                tess.setColorRGBA_F(1.0F, 1.0F, 1.0F, 0.4F)
//            }
//            tess.addVertexWithUV(offset3, l, 0.0D, br, mV)
//            tess.addVertexWithUV(offset1, 0.0D, 0.0D, br, mU)
//            tess.addVertexWithUV(offset2, 0.0D, 0.0D, tl, mU)
//            tess.addVertexWithUV(offset4, l, 0.0D, tl, mV)
//            tess.draw()
//        }
//
//        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F)
//
//        GL11.glPopMatrix()
//
//        tess.startDrawingQuads()
//    }
//}