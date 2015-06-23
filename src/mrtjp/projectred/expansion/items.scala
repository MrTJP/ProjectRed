/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import java.util.{List => JList}

import codechicken.lib.vec.BlockCoord
import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.core.item.ItemCore
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.api.IScrewdriver
import net.minecraft.entity.Entity
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemArmor.ArmorMaterial
import net.minecraft.item.{Item, ItemArmor, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.world.World

trait TItemBattery extends IChargable
{
    def isEmpty:Boolean
    def nonEmpty = !isEmpty

    def getEmptyVariant:Item
    def getChargedVariant:Item

    override def addPower(stack:ItemStack, pow:Int):(ItemStack, Int) =
    {
        stack.getItem match
        {
            case b:TItemBattery if pow > 0 =>
                val newStack = if (b.isEmpty) new ItemStack(b.getChargedVariant, 1, b.getChargedVariant.getMaxDamage) else stack
                val spaceLeft = newStack.getItemDamage
                val toAdd = math.min(spaceLeft, pow)
                newStack.setItemDamage(newStack.getItemDamage-toAdd)
                (newStack, toAdd)
            case _ => (stack, 0)
        }
    }

    override def drawPower(stack:ItemStack, pow:Int):(ItemStack, Int) =
    {
        stack.getItem match
        {
            case b:TItemBattery if b.nonEmpty =>
                val powerLeft = stack.getMaxDamage-stack.getItemDamage
                val toDraw = math.min(powerLeft, pow)
                stack.setItemDamage(stack.getItemDamage+toDraw)
                val newStack = if (stack.getItemDamage >= stack.getMaxDamage) new ItemStack(b.getEmptyVariant) else stack
                (newStack, toDraw)
            case _ => (stack, 0)
        }
    }

    override def isFullyCharged(stack:ItemStack) =
        stack.getItemDamage == 0 && stack.getItem == getChargedVariant
}

class ItemBatteryEmpty extends ItemCore("projectred.expansion.emptybattery") with TItemBattery
{
    setCreativeTab(ProjectRedExpansion.tabExpansion)
    setTextureName("projectred:emptybattery")

    override def isEmpty = true

    override def getEmptyVariant = this
    override def getChargedVariant = ProjectRedExpansion.itemBattery
}

class ItemBattery extends ItemCore("projectred.expansion.battery") with TItemBattery
{
    setMaxDamage(1600)
    setNoRepair()
    setMaxStackSize(1)
    setTextureName("projectred:battery")
    setCreativeTab(ProjectRedExpansion.tabExpansion)

    override def isEmpty = false

    override def getEmptyVariant = ProjectRedExpansion.itemEmptybattery
    override def getChargedVariant = this
}

trait IChargable
{
    def addPower(stack:ItemStack, pow:Int):(ItemStack, Int) =
    {
        stack.getItem match
        {
            case b:IChargable if pow > 0 =>
                val spaceLeft = stack.getItemDamage
                val toAdd = math.min(spaceLeft, pow)
                stack.setItemDamage(stack.getItemDamage-toAdd)
                (stack, toAdd)
            case _ => (stack, 0)
        }
    }

    def drawPower(stack:ItemStack, pow:Int):(ItemStack, Int) =
    {
        stack.getItem match
        {
            case b:IChargable =>
                val powerLeft = stack.getMaxDamage-stack.getItemDamage
                val toDraw = math.min(powerLeft, pow)
                stack.setItemDamage(stack.getItemDamage+toDraw)
                (stack, toDraw)
            case _ => (stack, 0)
        }
    }

    def isFullyCharged(stack:ItemStack) = stack.getItemDamage == 0
}

class ItemElectronicScrewdriver extends ItemCore("projectred.expansion.electric_screwdriver") with IScrewdriver with IChargable
{
    setMaxStackSize(1)
    setMaxDamage(400)
    setNoRepair()
    setCreativeTab(ProjectRedExpansion.tabExpansion)
    setTextureName("projectred:electric_screwdriver")

    override def onItemUse(stack:ItemStack, player:EntityPlayer, w:World,
                           x:Int, y:Int, z:Int, side:Int,
                           par8:Float, par9:Float, par10:Float) = false

    override def doesSneakBypassUse(world:World, x:Int, y:Int, z:Int,
                                    player:EntityPlayer) = true

    override def canUse(player:EntityPlayer, stack:ItemStack) = stack.getItemDamage < stack.getMaxDamage

    override def damageScrewdriver(player:EntityPlayer, stack:ItemStack)
    {
        stack.damageItem(1, player)
    }
}

class ItemElectronicJetpack extends ItemArmor(ArmorMaterial.DIAMOND, 0, 1) with IChargable
{
    setMaxStackSize(1)
    setMaxDamage(6400)
    setNoRepair()
    setCreativeTab(ProjectRedExpansion.tabExpansion)
    setTextureName("projectred:jetpack")
    setUnlocalizedName("projectred.expansion.jetpack")
    GameRegistry.registerItem(this, "projectred.expansion.jetpack")

    override def onArmorTick(world:World, player:EntityPlayer, stack:ItemStack)
    {
        if (SpacebarServerTracker.isKeyDown(player) && stack.getItemDamage < stack.getMaxDamage)
        {
            propellPlayer(player, stack)
            stack.setItemDamage(stack.getItemDamage+16)
        }
    }

    override def getArmorTexture(stack:ItemStack, entity:Entity, slot:Int, t:String) =
        "projectred:textures/items/jetpack_1.png"

    def propellPlayer(player:EntityPlayer, stack:ItemStack)
    {
        val maxHeight = 256.0*0.6
        val thrust = 0.1
        val maxUpSpeed = 0.55
        val stabalizeSpeed = 0.16
        val heightFalloff = 25.0
        val damageFalloff = 200.0

        var power = 1.0

        val y = player.posY
        if (y > maxHeight-heightFalloff)
            power *= math.max(0, maxHeight-y)/heightFalloff

        val damage = stack.getItemDamage
        if (damage > getMaxDamage-damageFalloff)
            power *= math.max(0, getMaxDamage-damage)/heightFalloff

        val velY = player.motionY
        val accelY = if (player.isSneaking) math.min(-velY*stabalizeSpeed, thrust*power) else thrust*power
        player.motionY = math.min(velY+accelY, maxUpSpeed)

        if (ForwardServerTracker.isKeyDown(player))
            player.moveFlying(0, (power*0.6).toFloat, 0.055f)

        player.distanceWalkedModified = 0
        player.fallDistance =
                if (player.motionY < 0) ((player.motionY*player.motionY)/0.065).toFloat
                else 0
    }
}

class ItemInfusedEnderPearl extends ItemCore("projectred.expansion.infused_ender_pearl")
{
    setMaxStackSize(1)
    setTextureName("projectred:infused_ender_pearl")
    setCreativeTab(ProjectRedExpansion.tabExpansion)

    override def addInformation(stack:ItemStack, player:EntityPlayer, list:JList[_], flag:Boolean)
    {
        import ItemInfusedEnderPearl._
        import net.minecraft.util.EnumChatFormatting._
        val slist = list.asInstanceOf[JList[String]]
        if (hasLocation(stack))
        {
            val bc = getLocation(stack)
            slist.add(GRAY+s"Tied to [${bc.x}, ${bc.y}, ${bc.z}]")
        }
    }
}

object ItemInfusedEnderPearl
{
    private def assertNBT(stack:ItemStack)
    {
        if (!stack.hasTagCompound)
            stack.setTagCompound(new NBTTagCompound)
    }

    def setLocation(stack:ItemStack, x:Int, y:Int, z:Int)
    {
        assertNBT(stack)
        val tag = stack.getTagCompound
        tag.setInteger("locX", x)
        tag.setInteger("locY", y)
        tag.setInteger("locZ", z)
    }

    def getLocation(stack:ItemStack) =
    {
        assertNBT(stack)
        val tag = stack.getTagCompound
        new BlockCoord(tag.getInteger("locX"), tag.getInteger("locY"), tag.getInteger("locZ"))
    }

    def hasLocation(stack:ItemStack) =
        stack.hasTagCompound && stack.getTagCompound.hasKey("locX")
}