/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.{Cuboid6, Vector3}
import com.google.common.base.Predicate
import mrtjp.core.fx.ParticleAction._
import mrtjp.core.fx.particles.{BeamPulse2, SpriteParticle}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.expansion.item.InfusedEnderPearlItem
import net.minecraft.block.BlockState
import net.minecraft.client.Minecraft
import net.minecraft.client.world.ClientWorld
import net.minecraft.entity.item.{EnderPearlEntity, ItemEntity}
import net.minecraft.entity.{Entity, EntityType, LivingEntity}
import net.minecraft.item.{ItemStack, Items}
import net.minecraft.nbt.{CompoundNBT, INBT}
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{Direction, ResourceLocation}
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.common.capabilities.Capability.IStorage
import net.minecraftforge.common.capabilities._
import net.minecraftforge.common.util.{LazyOptional, NonNullSupplier}
import net.minecraftforge.event.AttachCapabilitiesEvent
import net.minecraftforge.eventbus.api.SubscribeEvent

import java.util.concurrent.Callable
import scala.jdk.CollectionConverters._

class TileTeleposer extends TileMachine(ExpansionContent.teleposerTile.get) with TPoweredMachine
{
    var isCharged = false
    var storage = 0

    override def saveToNBT(tag:CompoundNBT) = {
        super.saveToNBT(tag)
        tag.putInt("storage", storage)
    }

    override def loadFromNBT(tag:CompoundNBT) = {
        super.loadFromNBT(tag)
        storage = tag.getInt("storage")
        isCharged = storage >= getTransportDraw
    }

    override def writeDesc(out:MCDataOutput):Unit = {
        super.writeDesc(out)
        out.writeBoolean(isCharged)
    }

    override def readDesc(in:MCDataInput):Unit = {
        super.readDesc(in)
        isCharged = in.readBoolean()
    }

    override def readUpdate(key:Int, in:MCDataInput):Unit = key match {
        case 2 => //doTransformFX()
        case 3 =>
            isCharged = in.readBoolean()
//            markRender()
        case _ => super.readUpdate(key, in)
    }

    def sendTransformFX():Unit = {
        sendUpdate(2, _ => ())
    }

    def sendICUpdate():Unit = {
        sendUpdate(3, _.writeBoolean(storage >= getTransportDraw))
    }

    def getMaxStorage = 16000
    def getDrawSpeed = 800
    def getDrawCeil = 600
    def getTransportDraw = 8000

    override def updateServer():Unit = {
        super.updateServer()

        if (storage >= getTransportDraw) {
            updateHeldItems()
            updateOrbits()
            if (getLevel.getGameTime%20 == 0) {
                tryInfusePearlItem()
                tryTransportEnderProjectile()
            }
        }

        if (cond.charge > getDrawCeil && storage < getMaxStorage)  {
            var n = math.min(cond.charge-getDrawCeil, getDrawSpeed)/10
            n = math.min(n, getMaxStorage-storage)
            cond.drawPower(n*1000)
            storage += n
        }

        if (getLevel.getGameTime%10 == 0) updateRendersIfNeeded()
    }

    override def updateClient():Unit = {
        super.updateClient()

        if (isCharged) {
            updateOrbits()
            updateHeldItems()
            doPearlBeamFX()
            doRandomSparklies()
        }
    }

    def updateRendersIfNeeded():Unit = {
        val ic2 = storage >= getTransportDraw
        if (isCharged != ic2) {
            sendICUpdate()
            pushState()
        }
        isCharged = ic2
    }

    override def covertToBlockState(state: BlockState): BlockState = {
        super.covertToBlockState(state)
            .setValue(BaseMachineBlock.CHARGED_PROPERTY, Boolean.box(isCharged))
    }

    def updateOrbits():Unit = {
        for (e <- getProjectilesToOrbit)
            makeEntityOrbit(e)
    }

    def updateHeldItems():Unit = {
        for (e <- getAllItemEntities)
            makeEntityHeld(e)
    }

    def tryInfusePearlItem():Unit = {
        getProminentHeldItem match {
            case Some(ei) if ei.getItem.getItem == Items.ENDER_PEARL && ei.getItem.getCount == 1 =>
                ei.remove()
                val stack = new ItemStack(ExpansionContent.infusedEnderPearlItem.get)
                InfusedEnderPearlItem.setLocation(stack, getBlockPos)

                val ent = new ItemEntity(getLevel, ei.getX, ei.getY, ei.getZ, stack)
                ent.setPickUpDelay(20)
                //ent.age = ei.age //TODO
                ent.bobOffs = ei.bobOffs

                level.addFreshEntity(ent)
                sendTransformFX()

            case _ =>
        }
    }

    def tryTransportEnderProjectile():Unit = {
        getDestination match {
            case Some(dest) if storage > getTransportDraw => level.getBlockEntity(dest) match {
                case te:TileTeleposer if te.storage >= te.getTransportDraw => te.getDestination match {
                    case Some(thatDest) if thatDest.equals(dest) => getProminentEnderProjectile match {
                        case Some(ep) if ep.getOwner.isInstanceOf[LivingEntity] =>
                            ep.remove()
                            val newEP = new EnderPearlEntity(level, ep.getOwner.asInstanceOf[LivingEntity])
                            newEP.setDeltaMovement(0, 0.1, 0)
                            newEP.setPos(dest.getX, dest.getY + 1, dest.getZ)
                            CapabilityTeleposedEnderPearl.setTeleposed(newEP)
                            level.addFreshEntity(newEP)
                            te.getProminentHeldItem match {
                                case Some(destHeld) => destHeld.setPickUpDelay(80)
                                case _ =>
                            }
                            storage -= getTransportDraw
                            te.storage -= te.getTransportDraw
                        case _ =>
                    }
                    case _ =>
                }
                case _ =>
            }
            case _ =>
        }
    }

    def getDestination:Option[BlockPos] = {
        getProminentHeldItem match {
            case Some(ei) if ei.getItem.getItem.isInstanceOf[InfusedEnderPearlItem] && InfusedEnderPearlItem.hasLocation(ei.getItem) =>
                Some(InfusedEnderPearlItem.getLocation(ei.getItem))
            case _ => None
        }
    }

    def getProminentHeldItem:Option[ItemEntity] = {
        val entities = getAllItemEntities
        if (entities.length == 1) Some(entities(0)) else None
    }

    def getProminentEnderProjectile:Option[EnderPearlEntity] = {
        val box = Cuboid6.full.copy.add(new Vector3(getBlockPos.getX, getBlockPos.getY+1, getBlockPos.getZ)).aabb

        level.getEntities(EntityType.ENDER_PEARL, box, new Predicate[EnderPearlEntity] {
            override def apply(t:EnderPearlEntity):Boolean =
                !CapabilityTeleposedEnderPearl.isTeleposed(t)
        }).asScala.headOption
    }

    def getProjectilesToOrbit:Array[EnderPearlEntity] = {
        val box = new Cuboid6(-3, 0, -3, 4, 4, 4).add(Vector3.fromBlockPos(getBlockPos)).aabb
        level.getEntities(EntityType.ENDER_PEARL, box, new Predicate[EnderPearlEntity] {
            override def apply(t:EnderPearlEntity):Boolean = !CapabilityTeleposedEnderPearl.isTeleposed(t)
        }).asScala.toArray
    }

    def getAllItemEntities:Array[ItemEntity] = {
        Cuboid6.full = new Cuboid6(0, 0, 0, 1, 1, 1)
        val box = Cuboid6.full.copy.add(new Vector3(getBlockPos.getX, getBlockPos.getY+1, getBlockPos.getZ)).aabb
        level.getEntities(EntityType.ITEM, box, new Predicate[ItemEntity] {
            override def apply(t:ItemEntity):Boolean = t.getItem.getItem == Items.ENDER_PEARL ||
                    t.getItem.getItem == ExpansionContent.infusedEnderPearlItem.get
        }).asScala.toArray
    }

    def makeEntityOrbit(e:EnderPearlEntity):Unit = {
        val target = Vector3.CENTER.copy.add(getBlockPos.getX, getBlockPos.getY+1, getBlockPos.getZ)
        val rpos = new Vector3(e.getX, e.getY, e.getZ).subtract(target)
        val targetDistance = math.max(0.35, math.sqrt(rpos.x*rpos.x+rpos.z*rpos.z)*0.97)
        val targetHeight = rpos.y*0.97
        val orbitSpeed = 0.3
        var orbitAngle = math.atan2(rpos.z, rpos.x)+orbitSpeed
        if (orbitAngle > math.Pi*2) orbitAngle -= math.Pi*2
        else if (orbitAngle < 0) orbitAngle += math.Pi*2

        if (e.getY > target.y) e.setPos(e.getX, e.getY - 0.05, e.getZ)
        else if (e.getY < target.y) e.setPos(e.getX, e.getY + 0.05, e.getZ)

        e.setDeltaMovement(0, 0, 0)

        e.setPos(
            target.x+math.cos(orbitAngle)*targetDistance,
            target.y+targetHeight,
            target.z+math.sin(orbitAngle)*targetDistance
        )
    }

    def makeEntityHeld(e:ItemEntity):Unit = {
        val target = Vector3.CENTER.copy.add(getBlockPos.getX, getBlockPos.getY+0.6, getBlockPos.getZ)
        val rpos = new Vector3(e.getX, e.getY, e.getZ).subtract(target)
        val targetPos = rpos.multiply(0.80).add(target)

        e.setDeltaMovement(0, 0, 0)

        val maxAge = e.getItem.getItem.getEntityLifespan(e.getItem, e.level)
        e.lifespan = e.getAge + maxAge

        e.setPos(targetPos.x, targetPos.y, targetPos.z)
    }

    private var beams:AnyRef = null

    @OnlyIn(Dist.CLIENT)
    def doPearlBeamFX():Unit = {
        if (beams == null) beams = Array[BeamPulse2]()
        var beams2 = beams.asInstanceOf[Array[BeamPulse2]]

        val elist = getProjectilesToOrbit
        while (beams2.length < elist.size) beams2 :+= null
        beams = beams2

        val source = Vector3.CENTER.copy.add(getBlockPos.getX, getBlockPos.getY+1, getBlockPos.getZ)

        for (i <- elist.indices) {
            var beam = beams2(i)
            if (beam == null || !beam.isAlive) {
                beam = new BeamPulse2(level.asInstanceOf[ClientWorld])
                beam.setLifetime(20)
                beam.alpha = 0.3f
                beam.setRGB(0.5, 0.5, 0.5)
                beams2(i) = beam
                Minecraft.getInstance().particleEngine.add(beam)
            }
            val ent = elist(i)

            beam.setAge(beam.getAge%beam.getLifetime)
            beam.setPos(source)
            beam.setTarget(ent.getX, ent.getY, ent.getZ)
            if (source.copy.subtract(new Vector3(ent.getX, ent.getY, ent.getZ)).mag() < 0.75)
                beam.doPulse(EnumColour.GREEN.rF, EnumColour.GREEN.gF, EnumColour.GREEN.bF)
        }

    }

    @OnlyIn(Dist.CLIENT)
    def doRandomSparklies():Unit = {
        if (level.getGameTime%15 == 0 && level.random.nextDouble() > 0.20) {
            val x = getBlockPos.getX
            val y = getBlockPos.getY
            val z = getBlockPos.getZ

            val p = new SpriteParticle(level.asInstanceOf[ClientWorld])
            p.setPos(new Vector3(x+0.5, y+1.0, z+0.5).add(new Vector3(1, 1, 1).multiply(level.random.nextDouble())))
            p.isImmortal = true
            p.texture = "projectred-core:textures/particles/flutter1.png"
            p.rgb = new Vector3(EnumColour.PINK.rF, EnumColour.PINK.gF, EnumColour.PINK.bF)
            p.scale = new Vector3(0, 0, 0)
            p.alpha = 0

            import mrtjp.core.fx.ParticleAction._
            val a = group(
                repeatForever(orbitAround(x+0.5, z+0.5, 0.30, 1)),
                sequence(
                    group(
                        changeAlphaTo(1, 5),
                        scaleTo(0.075, 0.075, 0.075, 20)
                    ),
                    group(
                        moveTo(x+0.5, y+1, z+0.5, 80),
                        sequence(
                            delay(45),
                            group(
                                changeAlphaTo(0, 20),
                                scaleTo(0, 0, 0, 5)
                            )
                        )
                    ),
                    kill()
                )
            )
            p.runAction(a)

            Minecraft.getInstance().particleEngine.add(p)
        }
    }

    @OnlyIn(Dist.CLIENT)
    def doTransformFX():Unit = {
        val x = getBlockPos.getX
        val y = getBlockPos.getY
        val z = getBlockPos.getZ

        for (i <- 0 until 16) {
            val start = new Vector3(x, y+1, z).add(Vector3.CENTER)
            val p = new SpriteParticle(level.asInstanceOf[ClientWorld])
            Minecraft.getInstance().particleEngine.add(p)
            p.setPos(start)
            p.isImmortal = true
            p.texture = "projectred-core:textures/particles/bubble.png"
            p.rgb = new Vector3(EnumColour.MAGENTA.rF, EnumColour.MAGENTA.gF, EnumColour.MAGENTA.bF)
            p.scale = new Vector3(0, 0, 0)
            val s = level.random.nextDouble()*0.1
            val a1 = sequence(
                group(
                    scaleTo(0.025+s, 0.025+s, 0.025+s, 5),
                    moveTo(start.x+level.random.nextDouble()-0.5, start.y+level.random.nextDouble()-0.5, start.z+level.random.nextDouble()-0.5, 10)
                ),
                sequence(
                    changeTexture("projectred-core:textures/particles/bubble_pop.png"),
                    scaleTo(0, 0, 0, 3)
                ),
                kill()
            )
            p.runAction(a1)
        }
    }
}

trait ITeleposedItem
{
    var isTeleposed:Boolean
}

class TeleposedProperty extends ITeleposedItem with ICapabilityProvider with ICapabilitySerializable[CompoundNBT]
{
    private val lazyOptional = LazyOptional.of[ITeleposedItem](new NonNullSupplier[ITeleposedItem] {
        override def get():ITeleposedItem = TeleposedProperty.this
    })

    override var isTeleposed = false

    override def getCapability[T](capability:Capability[T], facing:Direction):LazyOptional[T] = {
        if (capability == CapabilityTeleposedEnderPearl.teleposedEnderPearlCapability) lazyOptional.cast()
        else LazyOptional.empty()
    }

    override def serializeNBT():CompoundNBT = {
        val tag = new CompoundNBT
        tag.putBoolean("isTeleposed", isTeleposed)
        tag
    }

    override def deserializeNBT(nbt:CompoundNBT):Unit = {
        isTeleposed = nbt.getBoolean("isTeleposed")
    }
}

object CapabilityTeleposedEnderPearl
{
    val teleposedEnderPearlCapabilityID = new ResourceLocation(ProjectRedExpansion.MOD_ID, "teleposed_ender_pearl")

    @CapabilityInject(classOf[ITeleposedItem])
    var teleposedEnderPearlCapability:Capability[ITeleposedItem] = _

    def registerCapability():Unit = {
        //Register the capability with the manager
        CapabilityManager.INSTANCE.register(classOf[ITeleposedItem], new IStorage[ITeleposedItem] {
            override def writeNBT(capability:Capability[ITeleposedItem], instance:ITeleposedItem, side:Direction):INBT = null
            override def readNBT(capability:Capability[ITeleposedItem], instance:ITeleposedItem, side:Direction, nbt:INBT):Unit = {}
        }, new Callable[ITeleposedItem] {
            override def call():ITeleposedItem = null
        })

        //Subscribe to attachment events
        MinecraftForge.EVENT_BUS.register(this)
    }

    @SubscribeEvent
    def onAttachCapability(event:AttachCapabilitiesEvent[Entity]):Unit = {
        event.getObject match {
            case ei:EnderPearlEntity =>
                event.addCapability(teleposedEnderPearlCapabilityID, new TeleposedProperty)
            case _ =>
        }
    }


    def setTeleposed(e:EnderPearlEntity):Unit = {
        e.getCapability(teleposedEnderPearlCapability, null) match {
            case t:ITeleposedItem => t.isTeleposed = true
            case _ =>
        }
    }

    def isTeleposed(e:EnderPearlEntity):Boolean = {
        e.getCapability(teleposedEnderPearlCapability, null) match {
            case t:ITeleposedItem => t.isTeleposed
            case _ => false
        }
    }
}

//object RenderTeleposer extends SimpleBlockRenderer
//{
//    import org.apache.commons.lang3.tuple.Triple
//
//    import java.lang.{Boolean => JBool}
//
//    var bottom:TextureAtlasSprite = _
//    var top1:TextureAtlasSprite = _
//    var top2:TextureAtlasSprite = _
//    var side1:TextureAtlasSprite = _
//    var side2:TextureAtlasSprite = _
//
//    var iconT1:UVTransformation = _
//    var iconT2:UVTransformation = _
//
//    override def handleState(state: IExtendedBlockState, world: IBlockAccess, pos: BlockPos): IExtendedBlockState = {
//
//        world.getTileEntity(pos) match {
//            case t:TileTeleposer => state.withProperty(UNLISTED_CHARGED_PROPERTY, t.isCharged.asInstanceOf[JBool])
//            case _ => state
//        }
//    }
//
//    override def getWorldTransforms(state: IExtendedBlockState) = {
//        val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
//        Triple.of(0, 0, if (isCharged) iconT2 else iconT1)
//    }
//
//    override def getItemTransforms(stack: ItemStack) = Triple.of(0, 0, iconT1)
//    override def shouldCull() = true
//
//    override def registerIcons(map:TextureMap)
//    {
//        bottom = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/teleposer/bottom"))
//        top1 = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/teleposer/top1"))
//        top2 = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/teleposer/top2"))
//        side1 = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/teleposer/side1"))
//        side2 = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/teleposer/side2"))
//
//        iconT1 = new MultiIconTransformation(bottom, top1, side1, side1, side1, side1)
//        iconT2 = new MultiIconTransformation(bottom, top2, side2, side2, side2, side2)
//    }
//}