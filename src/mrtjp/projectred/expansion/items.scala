/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import java.util.{List => JList}

import codechicken.lib.math.MathHelper
import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.{Rotation, Translation, Vector3}
import mrtjp.core.fx.particles.SpriteParticle
import mrtjp.core.item.ItemCore
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.api.IScrewdriver
import net.minecraft.client.Minecraft
import net.minecraft.client.model.{ModelBiped, ModelRenderer}
import net.minecraft.enchantment.Enchantment.Rarity
import net.minecraft.enchantment.{Enchantment, EnchantmentHelper, EnumEnchantmentType}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.{Entity, EntityLivingBase}
import net.minecraft.init.Blocks
import net.minecraft.inventory.EntityEquipmentSlot
import net.minecraft.item.ItemArmor.ArmorMaterial
import net.minecraft.item.{Item, ItemArmor, ItemStack}
import net.minecraft.nbt.{NBTTagCompound, NBTTagList}
import net.minecraft.util.math.BlockPos
import net.minecraft.util.text.TextFormatting
import net.minecraft.util._
import net.minecraft.world.{IBlockAccess, World}
import net.minecraftforge.common.{ISpecialArmor, MinecraftForge}
import net.minecraftforge.common.ISpecialArmor.ArmorProperties
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import net.minecraftforge.fml.common.gameevent.TickEvent.{ClientTickEvent, Phase}
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.mutable.{Set => MSet}

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
                val toAdd = Math.min(spaceLeft, pow)
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
                val toDraw = Math.min(powerLeft, pow)
                stack.setItemDamage(stack.getItemDamage+toDraw)
                val newStack = if (stack.getItemDamage >= stack.getMaxDamage) new ItemStack(b.getEmptyVariant) else stack
                (newStack, toDraw)
            case _ => (stack, 0)
        }
    }

    override def isFullyCharged(stack:ItemStack) =
        stack.getItemDamage == 0 && stack.getItem == getChargedVariant
}

class ItemEmptyBattery extends ItemCore with TItemBattery
{
    setCreativeTab(ProjectRedExpansion.tabExpansion)

    override def isEmpty = true

    override def getEmptyVariant = this
    override def getChargedVariant = ProjectRedExpansion.itemBattery
}

class ItemBattery extends ItemCore with TItemBattery
{
    setCreativeTab(ProjectRedExpansion.tabExpansion)
    setMaxStackSize(1)
    setMaxDamage(1600)
    setNoRepair()

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
                val toAdd = Math.min(spaceLeft, pow)
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
                val toDraw = Math.min(powerLeft, pow)
                stack.setItemDamage(stack.getItemDamage+toDraw)
                (stack, toDraw)
            case _ => (stack, 0)
        }
    }

    def isFullyCharged(stack:ItemStack) = stack.getItemDamage == 0

    def canApplyElectricEnchantment(enchantment:Enchantment) = false
}

class ItemElectricScrewdriver extends ItemCore with IScrewdriver with IChargable
{
    setCreativeTab(ProjectRedExpansion.tabExpansion)
    setMaxStackSize(1)
    setMaxDamage(400)
    setNoRepair()

    override def onItemUse(stack: ItemStack, playerIn: EntityPlayer, worldIn: World,
                           pos: BlockPos, hand: EnumHand, facing: EnumFacing,
                           hitX: Float, hitY: Float, hitZ: Float): EnumActionResult = EnumActionResult.PASS

    override def doesSneakBypassUse(stack: ItemStack, world: IBlockAccess, pos: BlockPos, player: EntityPlayer) = true

    override def canUse(player:EntityPlayer, stack:ItemStack) = stack.getItemDamage < stack.getMaxDamage

    override def damageScrewdriver(player:EntityPlayer, stack:ItemStack)
    {
        stack.damageItem(1, player)
    }
}

class ItemPlan extends ItemCore
{
    setCreativeTab(ProjectRedExpansion.tabExpansion)

    override def addInformation(stack:ItemStack, player:EntityPlayer, list:JList[String], flag:Boolean)
    {
        if (ItemPlan.hasRecipeInside(stack))
        {
            val s = s"${TextFormatting.BLUE}Output: ${TextFormatting.GRAY+ItemPlan.loadPlanOutput(stack).getDisplayName}"
            list.add(s)
        }
    }
}

object ItemPlan
{
    private def assertStackTag(stack:ItemStack)
    {
        if (!stack.hasTagCompound)
            stack.setTagCompound(new NBTTagCompound)
    }

    def hasRecipeInside(stack:ItemStack) =
    {
        stack.hasTagCompound && stack.getTagCompound.hasKey("recipe")
    }

    def savePlan(stack:ItemStack, inputs:Array[ItemStack], out:ItemStack)
    {
        assertStackTag(stack)
        val tag0 = new NBTTagList
        for (i <- 0 until 9)
        {
            val tag1 = new NBTTagCompound
            val slotStack = inputs(i)
            if (slotStack != null)
                slotStack.writeToNBT(tag1)
            tag0.appendTag(tag1)
        }
        val tag1 = new NBTTagCompound
        out.writeToNBT(tag1)
        tag0.appendTag(tag1)
        stack.getTagCompound.setTag("recipe", tag0)
    }

    def loadPlanInputs(stack:ItemStack) =
    {
        val out = new Array[ItemStack](9)
        val tag0 = stack.getTagCompound.getTagList("recipe", 10)
        for (i <- 0 until 9)
        {
            val tag1 = tag0.getCompoundTagAt(i)
            if (tag1.hasKey("id"))
                out(i) = ItemStack.loadItemStackFromNBT(tag1)
        }
        out
    }

    def loadPlanOutput(stack:ItemStack) =
    {
        val tag0 = stack.getTagCompound.getTagList("recipe", 10)
        val out = ItemStack.loadItemStackFromNBT(tag0.getCompoundTagAt(9))
        if (out != null) out else new ItemStack(Blocks.FIRE)//TODO Fire isnt an item anymore..
    }
}

class ItemJetpack extends ItemArmor(ArmorMaterial.DIAMOND, 0, EntityEquipmentSlot.CHEST) with IChargable with ISpecialArmor
{
    setCreativeTab(ProjectRedExpansion.tabExpansion)
    setMaxStackSize(1)
    setMaxDamage(6400)
    setNoRepair()

    override def canApplyElectricEnchantment(enchantment:Enchantment) = enchantment match {
        case ench:EnchantmentElectricEfficiency => true
        case _ => false
    }

    override def onArmorTick(world:World, player:EntityPlayer, stack:ItemStack)
    {
        if (SpacebarServerTracker.isKeyDown(player) && stack.getItemDamage < stack.getMaxDamage)
        {
            propellPlayer(player, stack)
            if (!player.capabilities.isCreativeMode)
                stack.setItemDamage(stack.getItemDamage+getPowerDraw(stack))
            ItemJetpack.setStateOfEntity(player.getEntityId, true, !world.isRemote)
        }
        else
            ItemJetpack.setStateOfEntity(player.getEntityId, false, !world.isRemote)
    }

    def getPowerDraw(stack:ItemStack):Int =
    {
        if (!stack.isItemEnchanted) return 16
        val i = EnchantmentHelper.getEnchantmentLevel(ProjectRedExpansion.enchantmentElectricEfficiency, stack)
        Math.max(1, 16-i*3)
    }

    override def getArmorTexture(stack:ItemStack, entity:Entity, slot:EntityEquipmentSlot, t:String) =
        "projectred:textures/items/mechanical/jetpack_1.png"

    @SideOnly(Side.CLIENT)
    override def getArmorModel(entityLiving: EntityLivingBase, itemStack: ItemStack, armorSlot: EntityEquipmentSlot, _default: ModelBiped):ModelBiped = ModelJetpack

    override def damageArmor(entity:EntityLivingBase, stack:ItemStack, source:DamageSource, damage:Int, slot:Int){}

    override def getArmorDisplay(player:EntityPlayer, armor:ItemStack, slot:Int) = 0

    override def getProperties(player:EntityLivingBase, armor:ItemStack, source:DamageSource, damage:Double, slot:Int) =
        new ArmorProperties(0, 1, 0)

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
            power *= Math.max(0, maxHeight-y)/heightFalloff

        val damage = stack.getItemDamage
        if (damage > getMaxDamage-damageFalloff)
            power *= Math.max(0, getMaxDamage-damage)/heightFalloff

        val velY = player.motionY
        val accelY = if (player.isSneaking) Math.min(-velY*stabalizeSpeed, thrust*power) else thrust*power
        player.motionY = Math.min(velY+accelY, maxUpSpeed)

        if (ForwardServerTracker.isKeyDown(player))
            player.moveRelative(0, (power*0.6).toFloat, 0.055f)

        player.distanceWalkedModified = 0
        player.fallDistance = if (player.motionY < 0)
            ((player.motionY*player.motionY)/0.065).toFloat else 0
    }

}

trait IElectricEnchantment extends Enchantment
{
    override def canApply(stack:ItemStack) = stack.getItem match {
        case ich:IChargable =>
            super.canApply(stack) && ich.canApplyElectricEnchantment(this)
        case _ => false
    }
}

class EnchantmentElectricEfficiency extends Enchantment(Rarity.UNCOMMON, EnumEnchantmentType.ALL, Array(EntityEquipmentSlot.CHEST)) with IElectricEnchantment
{
    override def getMinLevel = 1
    override def getMaxLevel = 4
}

object ItemJetpack
{
    var entitiesUsingJetpack = Set[Int]()

    def setStateOfEntity(id:Int, state:Boolean, isServer:Boolean)
    {
        val prev = isEntityUsing(id)
        if (state) entitiesUsingJetpack += id
        else entitiesUsingJetpack -= id
        if (isServer && prev != isEntityUsing(id))
        {
            val packet = new PacketCustom(ExpansionSPH.channel, ExpansionSPH.jetpack_state)
            packet.writeInt(id).writeBoolean(state).sendToClients()
        }
    }

    def isEntityUsing(id:Int) = entitiesUsingJetpack.contains(id)

    @SubscribeEvent
    @SideOnly(Side.CLIENT)
    def onRenderTick(event:ClientTickEvent)
    {
        if (event.phase == Phase.END)
            for (id <- entitiesUsingJetpack)
                Minecraft.getMinecraft.theWorld.getEntityByID(id) match
                {
                    case e:EntityPlayer => renderParticlesForPlayer(e)
                    case _ =>
                }
    }

    @SideOnly(Side.CLIENT)
    def register()
    {
        MinecraftForge.EVENT_BUS.register(this)
    }

    @SideOnly(Side.CLIENT)
    def renderParticlesForPlayer(player:EntityPlayer)
    {
        val pos1 = new Vector3(-2.5/16D, -15/16D, -4/16D).apply(new Rotation(-player.renderYawOffset * MathHelper.torad, 0, 1, 0) `with`
                new Translation(player.posX, player.posY, player.posZ))
        val pos2 = new Vector3(2.5/16D, -15/16D, -4/16D).apply(new Rotation(-player.renderYawOffset * MathHelper.torad, 0, 1, 0) `with`
                new Translation(player.posX, player.posY, player.posZ))

        val positions = Seq(pos1, pos2)

        val s = -player.worldObj.rand.nextDouble()*0.2+Math.min(0, player.motionY)

        import mrtjp.core.fx.ParticleAction._
        val a1 = group(
            repeatForever(sequence(
                delay(2.5),
                changeTexture("projectred:textures/particles/flutter0.png"),
                delay(2.5),
                changeTexture("projectred:textures/particles/flutter1.png"),
                delay(2.5),
                changeTexture("projectred:textures/particles/flutter2.png"),
                delay(2.5),
                changeTexture("projectred:textures/particles/flutter3.png")
            )),
            sequence(
                moveFor(0, s, 0, 10),
                group(
                    scaleTo(0, 0, 0, 10),
                    moveFor(0, s, 0, 10)
                ),
                kill()
            )
        )

        for (pos <- positions) for (i <- 0 until 2)
        {
            val p = new SpriteParticle(player.worldObj)
            Minecraft.getMinecraft.effectRenderer.addEffect(p)
            val r = player.worldObj.rand
            p.setPos(pos.copy.add(new Vector3(r.nextDouble(), r.nextDouble(), r.nextDouble())
                    .subtract(Vector3.center).multiply(4/16D)))
            p.setMaxAge(50)
            //p.noClip = false
            p.texture = "projectred:textures/particles/box.png"
            p.rgb = new Vector3(0, 198/256D, 210/256D)
            p.scale = new Vector3(0.05, 0.05, 0.05)
            p.runAction(a1)
        }
    }
}

class ItemInfusedEnderPearl extends ItemCore
{
    setMaxStackSize(1)

    override def addInformation(stack:ItemStack, player:EntityPlayer, list:JList[String], flag:Boolean)
    {
        import ItemInfusedEnderPearl._
        val slist = list.asInstanceOf[JList[String]]
        if (hasLocation(stack))
        {
            val bc = getLocation(stack)
            slist.add(TextFormatting.GRAY+s"Tied to [${bc.getX}, ${bc.getY}, ${bc.getZ}]")
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
        new BlockPos(tag.getInteger("locX"), tag.getInteger("locY"), tag.getInteger("locZ"))
    }

    def hasLocation(stack:ItemStack) =
        stack.hasTagCompound && stack.getTagCompound.hasKey("locX")
}

object ModelJetpack extends ModelBiped(1.0F, 0, 64, 64)
{
    bipedBody.showModel = true
    bipedRightArm.showModel = true
    bipedLeftArm.showModel = true
    bipedHead.showModel = false
    bipedHeadwear.showModel = false
    bipedRightLeg.showModel = false
    bipedLeftLeg.showModel = false

    val middle = new ModelRenderer(this, 0, 54).setTextureSize(64, 64)
    middle.addBox(-2F, 3F, 3.6F, 4, 5, 2)
    middle.setRotationPoint(0F, 0F, 0F)
    middle.mirror = true
    setRotation(middle, 0F, 0F, 0F)

    val leftCanister = new ModelRenderer(this, 0, 32).setTextureSize(64, 64)
    leftCanister.addBox(0.5F, 0F, 2.6F, 4, 9, 4)
    leftCanister.setRotationPoint(0F, 0F, 0F)
    leftCanister.mirror = true
    setRotation(leftCanister, 0F, 0F, 0F)

    val rightCanister = new ModelRenderer(this, 17, 32).setTextureSize(64, 64)
    rightCanister.addBox(-4.5F, 0F, 2.6F, 4, 9, 4)
    rightCanister.setRotationPoint(0F, 0F, 0F)
    rightCanister.mirror = true
    setRotation(rightCanister, 0F, 0F, 0F)

    val leftTip1 = new ModelRenderer(this, 0, 46).setTextureSize(64, 64)
    leftTip1.addBox(1F, -1F, 3.1F, 3, 1, 3)
    leftTip1.setRotationPoint(0F, 0F, 0F)
    leftTip1.mirror = true
    setRotation(leftTip1, 0F, 0F, 0F)

    val rightTip1 = new ModelRenderer(this, 17, 46).setTextureSize(64, 64)
    rightTip1.addBox(-4F, -1F, 3.1F, 3, 1, 3)
    rightTip1.setRotationPoint(0F, 0F, 0F)
    rightTip1.mirror = true
    setRotation(rightTip1, 0F, 0F, 0F)

    val leftExhaust1 = new ModelRenderer(this, 35, 32).setTextureSize(64, 64)
    leftExhaust1.addBox(1F, 9F, 3.1F, 3, 1, 3)
    leftExhaust1.setRotationPoint(0F, 0F, 0F)
    leftExhaust1.mirror = true
    setRotation(leftExhaust1, 0F, 0F, 0F)

    val leftExhaust2 = new ModelRenderer(this, 35, 37).setTextureSize(64, 64)
    leftExhaust2.addBox(0.5F, 10F, 2.6F, 4, 3, 4)
    leftExhaust2.setRotationPoint(0F, 0F, 0F)
    leftExhaust2.mirror = true
    setRotation(leftExhaust2, 0F, 0F, 0F)

    val rightExhaust1 = new ModelRenderer(this, 48, 32).setTextureSize(64, 64)
    rightExhaust1.addBox(-4F, 9F, 3.1F, 3, 1, 3)
    rightExhaust1.setRotationPoint(0F, 0F, 0F)
    rightExhaust1.mirror = true
    setRotation(rightExhaust1, 0F, 0F, 0F)

    val rightExhaust2 = new ModelRenderer(this, 35, 46).setTextureSize(64, 64)
    rightExhaust2.addBox(-4.5F, 10F, 2.6F, 4, 3, 4)
    rightExhaust2.setRotationPoint(0F, 0F, 0F)
    rightExhaust2.mirror = true
    setRotation(rightExhaust2, 0F, 0F, 0F)

    bipedBody.addChild(middle)
    bipedBody.addChild(leftCanister)
    bipedBody.addChild(rightCanister)
    bipedBody.addChild(leftTip1)
    bipedBody.addChild(rightTip1)
    bipedBody.addChild(leftExhaust1)
    bipedBody.addChild(leftExhaust2)
    bipedBody.addChild(rightExhaust1)
    bipedBody.addChild(rightExhaust2)

    def setRotation(model:ModelRenderer, x:Float, y:Float, z:Float)
    {
        model.rotateAngleX = x
        model.rotateAngleY = y
        model.rotateAngleZ = z
    }
}
