package mrtjp.projectred.exploration

import codechicken.lib.util.ServerUtils
import codechicken.microblock.Saw
import com.google.common.collect.{HashMultimap, Multimap}
import mrtjp.core.gui.{GuiLib, NodeContainer}
import mrtjp.core.inventory.TInventory
import mrtjp.core.world.WorldLib
import mrtjp.projectred.core.ItemCraftingDamage
import mrtjp.projectred.exploration.ExplorationContent.{athameItemTier, explorationItemGroup}
import net.minecraft.block._
import net.minecraft.client.gui.screen.Screen
import net.minecraft.client.util.ITooltipFlag
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.ai.attributes.{Attribute, AttributeModifier, Attributes}
import net.minecraft.entity.monster.EndermanEntity
import net.minecraft.entity.player.{PlayerEntity, PlayerInventory, ServerPlayerEntity}
import net.minecraft.inventory.EquipmentSlotType
import net.minecraft.inventory.container.{ClickType, SimpleNamedContainerProvider, Slot}
import net.minecraft.item._
import net.minecraft.nbt.CompoundNBT
import net.minecraft.tags.BlockTags
import net.minecraft.util.math.BlockPos
import net.minecraft.util.text.{ITextComponent, StringTextComponent, TextFormatting}
import net.minecraft.util.{ActionResult, ActionResultType, DamageSource, Hand}
import net.minecraft.world.World
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}

import java.util.{Collections, List => JList}

class ItemBackpack extends Item(new Item.Properties().stacksTo(1).tab(ExplorationContent.explorationItemGroup)) {
    override def useOn(context: ItemUseContext) = {
        context.getPlayer match {
            case entity: ServerPlayerEntity => openGui(entity)
            case _ =>
        }
        ActionResultType.SUCCESS
    }

    override def use(world: World, player: PlayerEntity, hand: Hand) = {
        player match {
            case entity: ServerPlayerEntity => openGui(entity)
            case _ =>
        }
        ActionResult.success(player.getItemInHand(hand))
    }

    def openGui(player: ServerPlayerEntity) {
        val name = player.getMainHandItem.getDisplayName
        ServerUtils.openContainer(player, new SimpleNamedContainerProvider((id, inv, _) => new ContainerBackpack(id, inv), name))
    }

    @OnlyIn(Dist.CLIENT)
    override def appendHoverText(stack: ItemStack, world: World, list: JList[ITextComponent], flag: ITooltipFlag) {
        if (Screen.hasShiftDown) {
            //TODO, localization
            list.add(new StringTextComponent(TextFormatting.GRAY.toString + (if (ItemBackpack.hasBagInv(stack)) ItemBackpack.getNumberOfItems(stack) else 0) + "/27 slots used"))
        }
    }
}

object ItemBackpack {
    val oreDictionaryVal = "prbackpack"

    def hasBagInv(stack: ItemStack) = stack.hasTag && stack.getTag.contains("baginv")

    def getBagTag(stack: ItemStack) = stack.getOrCreateTag().getCompound("baginv")

    def saveBagTag(stack: ItemStack, tag: CompoundNBT) = stack.getOrCreateTag().put("baginv", tag)

    def getNumberOfItems(stack: ItemStack) = getBagTag(stack).getList("items", 10).size()
}

class ContainerBackpack(windowId: Int, inv: BagInventory, playerInv: PlayerInventory) extends NodeContainer(ExplorationContent.containerBackpack.get(), windowId) {
    def this(windowId: Int, playerInv: PlayerInventory) = this(windowId, new BagInventory(playerInv.player), playerInv)

    {
        for (((x, y), i) <- GuiLib.createSlotGrid(8, 18, 9, 3, 0, 0).zipWithIndex) {
            addSlot(new Slot(inv, i, x, y))
        }
        addPlayerInv(playerInv, 8, 86, (inv, i, x, y) => new Slot(inv, i, x, y) {
            override def mayPickup(playerIn:PlayerEntity):Boolean =
                i != playerInv.selected
        })
    }

    override def clicked(id:Int, dragType:Int, clickType:ClickType, player:PlayerEntity):ItemStack = {
        // Required because vanilla's shyte handling of number hotbar quick swap.
        // Apparently it only asks the target slot (the one hovered over) if it can accept the item, but it does not ask the
        // hotbar slot (corresponding to number pressed) if it can take the stack (which would be denied by the custom slot class's canTakeStack implemented above).
        // Additionally, if the target slot is empty, it calls that empty slot's canTakeStack. This is almost certainly
        // a bug. canTakeStack should be called on BOTH slots, and isItemValid should be called on BOTH slots with the other slots contents.
        if (id == player.inventory.selected || (clickType == ClickType.SWAP && dragType == player.inventory.selected))
            ItemStack.EMPTY
        else
            super.clicked(id, dragType, clickType, player)
    }
}

class BagInventory(player: PlayerEntity) extends TInventory {
    override protected val storage = Array.fill(27)(ItemStack.EMPTY)

    loadInventory()

    override def getMaxStackSize = 64

    override def nbtSaveName = ""

    private def loadInventory() {
        if (closeIfNoBag()) return
        loadInv(ItemBackpack.getBagTag(player.getMainHandItem))
    }

    private def saveInventory() {
        if (closeIfNoBag()) return
        val tag = new CompoundNBT()
        saveInv(tag)
        ItemBackpack.saveBagTag(player.getMainHandItem, tag)
    }

    override def setChanged() {
        saveInventory()
    }

    private def closeIfNoBag() = {
        val bag = player.getMainHandItem
        val hasBag = ExplorationContent.tagBackpacks.contains(bag.getItem)
        if (!hasBag) {
            player.closeContainer()
        }
        !hasBag
    }

    override def canPlaceItem(i: Int, stack: ItemStack): Boolean = {
        !ExplorationContent.tagBackpackDisallowed.contains(stack.getItem)
    }

    override def removeItemNoUpdate(slot: Int) =
        if (closeIfNoBag()) {
            ItemStack.EMPTY
        } else {
            super.removeItemNoUpdate(slot)
        }

    override def removeItem(slot: Int, count: Int) =
        if (closeIfNoBag()) {
            ItemStack.EMPTY
        } else {
            super.removeItem(slot, count)
        }

    override def setItem(slot: Int, item: ItemStack) {
        if (!closeIfNoBag()) super.setItem(slot, item)
    }

    override def dropInvContents(w: World, pos: BlockPos) {
        if (!closeIfNoBag()) super.dropInvContents(w, pos)
    }
}

class ItemSaw(val tier: IItemTier, properties: Item.Properties) extends ItemCraftingDamage(properties.durability(tier.getUses)) with Saw {
    override def getCuttingStrength(item: ItemStack) = tier.getLevel
}

class ItemSickle(val tier: IItemTier, attackDamage: Float, attackSpeed: Float, properties: Item.Properties) extends ToolItem(attackDamage, attackSpeed, tier, Collections.emptySet(), properties) {
    private val radiusLeaves = 1
    private val radiusCrops = 2

    override def getDestroySpeed(stack: ItemStack, state: BlockState) = {
        if (state.getBlock.isInstanceOf[LeavesBlock]) {
            speed
        } else {
            super.getDestroySpeed(stack, state)
        }
    }



    override def mineBlock(stack: ItemStack, w: World, state: BlockState, pos: BlockPos, ent: LivingEntity): Boolean = {
        val player = ent match {
            case p: PlayerEntity => p
            case _ => null
        }

        if (player != null && state != null) {
            if (state.is(BlockTags.LEAVES)) {
                return runLeaves(stack, w, pos, player)
            } else if (WorldLib.isPlantType(w, pos, state)) return runCrops(stack, w, pos, player)
        }
        super.mineBlock(stack, w, state, pos, ent)
    }

    private def runLeaves(stack: ItemStack, w: World, pos: BlockPos, player: PlayerEntity) = {
        var used = false
        for (i <- -radiusLeaves to radiusLeaves) for (j <- -radiusLeaves to radiusLeaves) for (k <- -radiusLeaves to radiusLeaves) {
            val p = pos.offset(i, j, k)
            val b = w.getBlockState(p)
            if (b != null && b.is(BlockTags.LEAVES)) {
                if (b.canHarvestBlock(w, p, player)) b.getBlock.playerDestroy(w, player, p, b, w.getBlockEntity(p), stack)
                w.removeBlock(p, false)
                used = true
            }
        }

        if (used) stack.hurtAndBreak(1, player, (p: PlayerEntity) => p.broadcastBreakEvent(EquipmentSlotType.MAINHAND))
        used
    }

    private def runCrops(stack: ItemStack, w: World, pos: BlockPos, player: PlayerEntity) = {
        var used = false
        for (i <- -radiusCrops to radiusCrops) for (j <- -radiusCrops to radiusCrops) {
            val p = pos.offset(i, 0, j)
            val b = w.getBlockState(p)
            if (b != null && WorldLib.isPlantType(w, pos, b)) {
                if (b.canHarvestBlock(w, p, player)) b.getBlock.playerDestroy(w, player, p, b, w.getBlockEntity(p), stack)
                w.removeBlock(p, false)
                used = true
            }
        }

        if (used) stack.hurtAndBreak(1, player, (p: PlayerEntity) => p.broadcastBreakEvent(EquipmentSlotType.MAINHAND))
        used
    }
}

class ItemWoolGin extends ItemCraftingDamage(new Item.Properties().durability(128).tab(ExplorationContent.explorationItemGroup))

/*class ItemLilySeeds extends ItemCore("projectred.exploration.lilyseed") with TItemSeed
{
    setHasSubtypes(true)
    setTextureName("projectred:world/lily_seed")
    setCreativeTab(ProjectRedExploration.tabExploration)

    override def getPlantBlock = (ProjectRedExploration.blockLily, 0)
    override def getPlantType(w:IBlockAccess, x:Int, y:Int, z:Int) = EnumPlantType.Plains

    override def getSubItems(item:Item, tab:CreativeTabs, list:JList[_])
    {
        for (i <- 0 until 16)
            list.asInstanceOf[JList[ItemStack]].add(new ItemStack(this, 1, i))
    }

    override def getColorFromItemStack(item:ItemStack, meta:Int) = Colors(item.getItemDamage).rgb

    override def onPlanted(item:ItemStack, w:World, x:Int, y:Int, z:Int)
    {
        if (!w.isRemote) w.getTileEntity(x, y, z) match
        {
            case te:TileLily => te.setupPlanted(item.getItemDamage)
            case _ =>
        }
    }
}*/

class ItemAthame extends SwordItem(athameItemTier, 3, -2.4F, new Item.Properties().tab(explorationItemGroup)) {
    private val standardDamage = 3.0f + athameItemTier.getAttackDamageBonus
    private val endermanDamage = standardDamage + 22.0f

    private var damage:Float = standardDamage

    override def getDestroySpeed(stack: ItemStack, block: BlockState) = 1.0F

    override def getDamage = damage



    override def hurtEnemy(stack: ItemStack, entity: LivingEntity, player: LivingEntity):Boolean = {
        damage = if (entity.isInstanceOf[EndermanEntity]) endermanDamage else standardDamage //TODO why is this necessary?

        val damageSource = player match {
            case p:PlayerEntity => DamageSource.playerAttack(p)
            case _ => DamageSource.mobAttack(player)
        }
        entity.hurt(damageSource, damage)

        val b = super.hurtEnemy(stack, entity, player)

        damage = standardDamage
        b
    }
    
    override def getAttributeModifiers(slot:EquipmentSlotType, stack:ItemStack) = {
        val modifiers = HashMultimap.create().asInstanceOf[Multimap[Attribute, AttributeModifier]]
        if (slot == EquipmentSlotType.MAINHAND) {
            modifiers.put(Attributes.ATTACK_DAMAGE, new AttributeModifier(Item.BASE_ATTACK_DAMAGE_UUID, "Weapon modifier", 0, AttributeModifier.Operation.ADDITION))
        }
        modifiers
    }
}
