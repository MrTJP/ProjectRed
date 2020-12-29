package mrtjp.projectred.exploration

import codechicken.lib.data.MCDataInput
import codechicken.lib.util.ServerUtils
import codechicken.microblock.Saw
import com.google.common.collect.{HashMultimap, Multimap}
import mrtjp.core.gui.{GuiLib, NodeContainer, Slot3}
import mrtjp.core.inventory.TInventory
import mrtjp.core.world.WorldLib
import mrtjp.projectred.core.ItemCraftingDamage
import mrtjp.projectred.exploration.ExplorationContent.{athameItemTier, explorationItemGroup}
import net.minecraft.block._
import net.minecraft.client.gui.screen.Screen
import net.minecraft.client.util.ITooltipFlag
import net.minecraft.entity.{LivingEntity, SharedMonsterAttributes}
import net.minecraft.entity.ai.attributes.AttributeModifier
import net.minecraft.entity.monster.EndermanEntity
import net.minecraft.entity.player.{PlayerEntity, PlayerInventory, ServerPlayerEntity}
import net.minecraft.inventory.EquipmentSlotType
import net.minecraft.inventory.container.SimpleNamedContainerProvider
import net.minecraft.item._
import net.minecraft.nbt.CompoundNBT
import net.minecraft.tags.BlockTags
import net.minecraft.util.math.BlockPos
import net.minecraft.util.text.{ITextComponent, StringTextComponent, TextFormatting}
import net.minecraft.util.{ActionResult, ActionResultType, DamageSource, Hand}
import net.minecraft.world.World

import java.util.{Collections, List => JList}

class ItemBackpack extends Item(new Item.Properties().maxStackSize(1).group(ExplorationContent.explorationItemGroup)) {
    override def onItemUse(context: ItemUseContext) = {
        context.getPlayer match {
            case entity: ServerPlayerEntity => openGui(entity)
            case _ =>
        }
        ActionResultType.SUCCESS
    }

    override def onItemRightClick(world: World, player: PlayerEntity, hand: Hand) = {
        player match {
            case entity: ServerPlayerEntity => openGui(entity)
            case _ =>
        }
        ActionResult.resultSuccess(player.getHeldItem(hand))
    }

    def openGui(player: ServerPlayerEntity) {
        val name = player.getHeldItemMainhand.getDisplayName
        ServerUtils.openContainer(player, new SimpleNamedContainerProvider((id, inv, _) => new ContainerBackpack(id, inv), name))
    }

    override def addInformation(stack: ItemStack, world: World, list: JList[ITextComponent], flag: ITooltipFlag) {
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
            val s = new Slot3(inv, i, x, y)
            addSlot(s)
        }
        addPlayerInv(playerInv, 8, 86)

        slots(playerInv.currentItem + 27).canRemoveDelegate = { () => false }
    }
}

class BagInventory(player: PlayerEntity) extends TInventory {
    override protected val storage = Array.fill(27)(ItemStack.EMPTY)

    loadInventory()

    override def getInventoryStackLimit = 64

    override def getName = ""

    private def loadInventory() {
        if (closeIfNoBag()) return
        loadInv(ItemBackpack.getBagTag(player.getHeldItemMainhand))
    }

    private def saveInventory() {
        if (closeIfNoBag()) return
        val tag = new CompoundNBT()
        saveInv(tag)
        ItemBackpack.saveBagTag(player.getHeldItemMainhand, tag)
    }

    override def markDirty() {
        saveInventory()
    }

    private def closeIfNoBag() = {
        val bag = player.getHeldItemMainhand
        val hasBag = ExplorationContent.tagBackpacks.contains(bag.getItem)
        if (!hasBag) {
            player.closeScreen()
        }
        !hasBag
    }

    override def isItemValidForSlot(i: Int, stack: ItemStack): Boolean = {
        !ExplorationContent.tagBackpackDisallowed.contains(stack.getItem)
    }

    override def removeStackFromSlot(slot: Int) =
        if (closeIfNoBag()) {
            ItemStack.EMPTY
        } else {
            super.removeStackFromSlot(slot)
        }

    override def decrStackSize(slot: Int, count: Int) =
        if (closeIfNoBag()) {
            ItemStack.EMPTY
        } else {
            super.decrStackSize(slot, count)
        }

    override def setInventorySlotContents(slot: Int, item: ItemStack) {
        if (!closeIfNoBag()) super.setInventorySlotContents(slot, item)
    }

    override def dropInvContents(w: World, pos: BlockPos) {
        if (!closeIfNoBag()) super.dropInvContents(w, pos)
    }
}

class ItemSaw(val tier: IItemTier, properties: Item.Properties) extends ItemCraftingDamage(properties.maxDamage(tier.getMaxUses)) with Saw {
    override def getCuttingStrength(item: ItemStack) = tier.getHarvestLevel
}

class ItemSickle(val tier: IItemTier, attackDamage: Float, attackSpeed: Float, properties: Item.Properties) extends ToolItem(attackDamage, attackSpeed, tier, Collections.emptySet(), properties) {
    private val radiusLeaves = 1
    private val radiusCrops = 2

    override def getDestroySpeed(stack: ItemStack, state: BlockState) = {
        if (state.getBlock.isInstanceOf[LeavesBlock]) {
            efficiency
        } else {
            super.getDestroySpeed(stack, state)
        }
    }

    override def onBlockDestroyed(stack: ItemStack, w: World, state: BlockState, pos: BlockPos, ent: LivingEntity): Boolean = {
        val player = ent match {
            case p: PlayerEntity => p
            case _ => null
        }

        if (player != null && state != null) {
            if (state.isIn(BlockTags.LEAVES)) {
                return runLeaves(stack, w, pos, player)
            } else if (WorldLib.isPlantType(w, pos, state)) return runCrops(stack, w, pos, player)
        }
        super.onBlockDestroyed(stack, w, state, pos, ent)
    }

    private def runLeaves(stack: ItemStack, w: World, pos: BlockPos, player: PlayerEntity) = {
        var used = false
        for (i <- -radiusLeaves to radiusLeaves) for (j <- -radiusLeaves to radiusLeaves) for (k <- -radiusLeaves to radiusLeaves) {
            val p = pos.add(i, j, k)
            val b = w.getBlockState(p)
            if (b != null && b.isIn(BlockTags.LEAVES)) {
                if (b.canHarvestBlock(w, p, player)) b.getBlock.harvestBlock(w, player, p, b, w.getTileEntity(p), stack)
                w.removeBlock(p, false)
                used = true
            }
        }

        if (used) stack.damageItem(1, player, (p: PlayerEntity) => p.sendBreakAnimation(EquipmentSlotType.MAINHAND))
        used
    }

    private def runCrops(stack: ItemStack, w: World, pos: BlockPos, player: PlayerEntity) = {
        var used = false
        for (i <- -radiusCrops to radiusCrops) for (j <- -radiusCrops to radiusCrops) {
            val p = pos.add(i, 0, j)
            val b = w.getBlockState(p)
            if (b != null && WorldLib.isPlantType(w, pos, b)) {
                if (b.canHarvestBlock(w, p, player)) b.getBlock.harvestBlock(w, player, p, b, w.getTileEntity(p), stack)
                w.removeBlock(p, false)
                used = true
            }
        }

        if (used) stack.damageItem(1, player, (p: PlayerEntity) => p.sendBreakAnimation(EquipmentSlotType.MAINHAND))
        used
    }
}

class ItemWoolGin extends ItemCraftingDamage(new Item.Properties().maxDamage(128).group(ExplorationContent.explorationItemGroup))

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

class ItemAthame extends SwordItem(athameItemTier, 3, -2.4F, new Item.Properties().group(explorationItemGroup)) {
    private var damage: Float = _

    override def getDestroySpeed(stack: ItemStack, block: BlockState) = 1.0F

    override def getAttackDamage = damage

    override def hitEntity(stack: ItemStack, entity: LivingEntity, player: LivingEntity) = {
        damage = athameItemTier.getAttackDamage

        if (entity.isInstanceOf[EndermanEntity]) damage = 25.0F else damage = 1.0F

        val damageSource = DamageSource.causePlayerDamage(player.asInstanceOf[PlayerEntity])
        entity.attackEntityFrom(damageSource, damage)

        super.hitEntity(stack, entity, player)
    }

    override def getAttributeModifiers(slot: EquipmentSlotType) = {
        val modifiers = HashMultimap.create().asInstanceOf[Multimap[String, AttributeModifier]]
        if (slot == EquipmentSlotType.MAINHAND) {
            modifiers.put(SharedMonsterAttributes.ATTACK_DAMAGE.getName, new AttributeModifier(Item.ATTACK_DAMAGE_MODIFIER, "Weapon modifier", 0, AttributeModifier.Operation.ADDITION))
        }
        modifiers
    }
}
