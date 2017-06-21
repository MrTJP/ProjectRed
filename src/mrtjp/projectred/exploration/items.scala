package mrtjp.projectred.exploration

import java.util.{List => JList}

import codechicken.lib.util.ItemUtils
import codechicken.microblock.Saw
import mrtjp.core.gui.{GuiLib, NodeContainer, Slot3}
import mrtjp.core.inventory.TInventory
import mrtjp.core.item.ItemCore
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExploration
import mrtjp.projectred.core.{ItemCraftingDamage, PartDefs}
import mrtjp.projectred.exploration.ArmorDefs.ArmorDef
import mrtjp.projectred.exploration.ToolDefs.ToolDef
import net.minecraft.block._
import net.minecraft.block.state.IBlockState
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.{Entity, EntityLivingBase}
import net.minecraft.init.{Blocks, Items}
import net.minecraft.inventory.EntityEquipmentSlot
import net.minecraft.item.Item.ToolMaterial
import net.minecraft.item.Item.ToolMaterial.{DIAMOND => toolMaterialDiamond, GOLD => toolMaterialGold, IRON => toolMaterialIron, STONE => toolMaterialStone, WOOD => toolMaterialWood}
import net.minecraft.item.ItemArmor.ArmorMaterial
import net.minecraft.item._
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.math.BlockPos
import net.minecraft.util.text.TextFormatting
import net.minecraft.util.{ActionResult, EnumActionResult, EnumFacing, EnumHand}
import net.minecraft.world.World
import net.minecraftforge.fml.common.registry.GameRegistry
import org.lwjgl.input.Keyboard

class ItemBackpack extends ItemCore
{
    setHasSubtypes(true)
    setMaxStackSize(1)
    setCreativeTab(ProjectRedExploration.tabExploration)

    override def onItemUse(stack:ItemStack, player:EntityPlayer, world:World, pos:BlockPos, hand:EnumHand, facing:EnumFacing, hitX:Float, hitY:Float, hitZ:Float) =
    {
        openGui(player)
        EnumActionResult.SUCCESS
    }

    override def onItemRightClick(stack:ItemStack, w:World, player:EntityPlayer, hand:EnumHand) =
    {
        openGui(player)
        ActionResult.newResult(EnumActionResult.SUCCESS, stack)
    }

    def openGui(player:EntityPlayer)
    {
        GuiBackpack.open(player, ItemBackpack.createContainer(player))
    }

    override def getSubItems(item:Item, tab:CreativeTabs, list:JList[ItemStack])
    {
        for (i <- 0 until 16)
            list.asInstanceOf[JList[ItemStack]].add(new ItemStack(this, 1, i))
    }

    override def addInformation(stack:ItemStack, player:EntityPlayer, list:JList[String], flag:Boolean)
    {
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
            list.asInstanceOf[JList[String]].add(
                TextFormatting.GRAY.toString+(if (ItemBackpack.hasBagInv(stack))
                    ItemBackpack.getNumberOfItems(stack) else 0)+"/27 slots used")
    }
}

object ItemBackpack
{
    val oreDictionaryVal = "prbackpack"

    def createContainer(player:EntityPlayer) =
        new ContainerBackpack(new BagInventory(player), player)

    def hasBagInv(stack:ItemStack) =
    {
        stack.hasTagCompound && stack.getTagCompound.hasKey("baginv")
    }

    def getBagTag(stack:ItemStack) =
    {
        stack.getTagCompound.getCompoundTag("baginv")
    }

    def saveBagTag(stack:ItemStack, tag:NBTTagCompound)
    {
        stack.getTagCompound.setTag("baginv", tag)
    }

    def getNumberOfItems(stack:ItemStack) =
    {
        getBagTag(stack).getTagList("items", 10).tagCount()
    }
}

class ContainerBackpack(inv:BagInventory, player:EntityPlayer) extends NodeContainer
{
    {
        for (((x, y), i) <- GuiLib.createSlotGrid(8, 18, 9, 3, 0, 0).zipWithIndex)
        {
            val s = new Slot3(inv, i, x, y)
            addSlotToContainer(s)
        }
        addPlayerInv(player, 8, 86)

        slots(player.inventory.currentItem+27).canRemoveDelegate = {() => false}
    }
}

class BagInventory(player:EntityPlayer) extends TInventory
{
    override protected val storage = new Array[ItemStack](27)

    loadInventory()

    override def getInventoryStackLimit = 64
    override def getName = ""

    private def loadInventory()
    {
        if (closeIfNoBag()) return
        loadInv(ItemBackpack.getBagTag(ItemUtils.getHeldStack(player)))
    }

    private def saveInventory()
    {
        if (closeIfNoBag()) return
        val tag = new NBTTagCompound
        saveInv(tag)
        ItemBackpack.saveBagTag(ItemUtils.getHeldStack(player), tag)
    }

    override def markDirty()
    {
        saveInventory()
    }

    private def closeIfNoBag() =
    {//TODO, Check hands ourself so we can dual wield, this should work for now tho.
        val bag = ItemUtils.getHeldStack(player)
        val hasBag = bag != null && bag.getItem == ProjectRedExploration.itemBackpack
        if (hasBag) {
            if (!bag.hasTagCompound)
                bag.setTagCompound(new NBTTagCompound)
        } else
            player.closeScreen()
        !hasBag
    }

    override def isItemValidForSlot(i:Int, stack:ItemStack):Boolean =
    {
        if (stack != null)
        {
            if (stack.getItem == ProjectRedExploration.itemBackpack) return false
            //for (blocked <- Configurator.backpackBlacklist) if (stack.itemID == blocked) return false
            //TODO backpack blacklist
            return true
        }
        false
    }

    override def removeStackFromSlot(slot:Int) =
        if (closeIfNoBag()) null
        else super.removeStackFromSlot(slot)

    override def decrStackSize(slot:Int, count:Int) =
        if (closeIfNoBag()) null
        else super.decrStackSize(slot, count)

    override def setInventorySlotContents(slot:Int, item:ItemStack)
    {
        if (!closeIfNoBag()) super.setInventorySlotContents(slot, item)
    }

    override def dropInvContents(w:World, pos:BlockPos)
    {
        if (!closeIfNoBag()) super.dropInvContents(w, pos)
    }
}

object ToolDefs
{
    private val wood = new ItemStack(Blocks.PLANKS)
    private val flint = new ItemStack(Items.FLINT)
    private val iron = new ItemStack(Items.IRON_INGOT)
    private val gold = new ItemStack(Items.GOLD_INGOT)
    private val ruby = PartDefs.RUBY.makeStack
    private val sapphire = PartDefs.SAPPHIRE.makeStack
    private val peridot = PartDefs.PERIDOT.makeStack
    private val diamond = new ItemStack(Items.DIAMOND)

    import mrtjp.projectred.ProjectRedExploration.{toolMaterialPeridot, toolMaterialRuby, toolMaterialSapphire}

    val RUBYAXE         = ToolDef("axeRuby",        "ruby_axe",         toolMaterialRuby,       ruby)
    val SAPPHIREAXE     = ToolDef("axeSapphire",    "sapphire_axe",     toolMaterialSapphire,   sapphire)
    val PERIDOTAXE      = ToolDef("axePeridot",     "peridot_axe",      toolMaterialPeridot,    peridot)

    val RUBYPICKAXE     = ToolDef("pickaxeRuby",    "ruby_pickaxe",     toolMaterialRuby,       ruby)
    val SAPPHIREPICKAXE = ToolDef("pickaxeSapphire","sapphire_pickaxe", toolMaterialSapphire,   sapphire)
    val PERIDOTPICKAXE  = ToolDef("pickaxePeridot", "peridot_pickaxe",  toolMaterialPeridot,    peridot)

    val RUBYSHOVEL      = ToolDef("shovelRuby",     "ruby_shovel",      toolMaterialRuby,       ruby)
    val SAPPHIRESHOVEL  = ToolDef("shovelSapphire", "sapphire_shovel",  toolMaterialSapphire,   sapphire)
    val PERIDOTSHOVEL   = ToolDef("shovelPeridot",  "peridot_shovel",   toolMaterialPeridot,    peridot)

    val RUBYSWORD       = ToolDef("swordRuby",      "ruby_sword",       toolMaterialRuby,       ruby)
    val SAPPHIRESWORD   = ToolDef("swordSapphire",  "sapphire_sword",   toolMaterialSapphire,   sapphire)
    val PERIDOTSWORD    = ToolDef("swordPeridot",   "peridot_sword",    toolMaterialPeridot,    peridot)

    val RUBYHOE         = ToolDef("hoeRuby",        "ruby_hoe",         toolMaterialRuby,       ruby)
    val SAPPHIREHOE     = ToolDef("hoeSapphire",    "sapphire_hoe",     toolMaterialSapphire,   sapphire)
    val PERIDOTHOE      = ToolDef("hoePeridot",     "peridot_hoe",      toolMaterialPeridot,    peridot)

    val WOODSAW         = ToolDef("sawWood",        "wooden_saw",       toolMaterialWood,       wood)
    val STONESAW        = ToolDef("sawStone",       "stone_saw",        toolMaterialStone,      flint)
    val IRONSAW         = ToolDef("sawIron",        "iron_saw",         toolMaterialIron,       iron)
    val GOLDSAW         = ToolDef("sawGold",        "golden_saw",       toolMaterialGold,       gold)
    val RUBYSAW         = ToolDef("sawRuby",        "ruby_saw",         toolMaterialRuby,       ruby)
    val SAPPHIRESAW     = ToolDef("sawSapphire",    "sapphire_saw",     toolMaterialSapphire,   sapphire)
    val PERIDOTSAW      = ToolDef("sawPeridot",     "peridot_saw",      toolMaterialPeridot,    peridot)
    val DIAMONDSAW      = ToolDef("sawDiamond",     "diamond_saw",      toolMaterialDiamond,    diamond)

    val WOODSICKLE      = ToolDef("sickleWood",     "wooden_sickle",    toolMaterialWood,       wood)
    val STONESICKLE     = ToolDef("sickleStone",    "stone_sickle",     toolMaterialStone,      flint)
    val IRONSICKLE      = ToolDef("sickleIron",     "iron_sickle",      toolMaterialIron,       iron)
    val GOLDSICKLE      = ToolDef("sickleGold",     "golden_sickle",    toolMaterialGold,       gold)
    val RUBYSICKLE      = ToolDef("sickleRuby",     "ruby_sickle",      toolMaterialRuby,       ruby)
    val SAPPHIRESICKLE  = ToolDef("sickleSapphire", "sapphire_sickle",  toolMaterialSapphire,   sapphire)
    val PERIDOTSICKLE   = ToolDef("sicklePeridot",  "peridot_sickle",   toolMaterialPeridot,    peridot)
    val DIAMONDSICKLE   = ToolDef("sickleDiamond",  "diamond_sickle",   toolMaterialDiamond,    diamond)

    case class ToolDef(unlocal:String, registryName:String, mat:ToolMaterial, repair:ItemStack)
}


trait TGemTool extends Item
{
    setCreativeTab(ProjectRedExploration.tabExploration)
    setUnlocalizedName("projectred.exploration."+toolDef.unlocal)
    GameRegistry.register(setRegistryName(toolDef.registryName))

    def toolDef:ToolDef

    override def getIsRepairable(ist1:ItemStack, ist2:ItemStack) =
    {
        if (toolDef.repair.isItemEqual(ist2)) true
        else false
    }
}

import mrtjp.projectred.exploration.ItemToolProxies._
class ItemGemAxe(override val toolDef:ToolDef, damage:Float, speed:Float) extends Axe(toolDef.mat, damage, speed) with TGemTool
class ItemGemPickaxe(override val toolDef:ToolDef) extends Pickaxe(toolDef.mat) with TGemTool
class ItemGemShovel(override val toolDef:ToolDef) extends Shovel(toolDef.mat) with TGemTool
class ItemGemSword(override val toolDef:ToolDef) extends Sword(toolDef.mat) with TGemTool
class ItemGemHoe(override val toolDef:ToolDef) extends ItemHoe(toolDef.mat) with TGemTool

class ItemGemSaw(val toolDef:ToolDef) extends ItemCraftingDamage with Saw
{
    setCreativeTab(ProjectRedExploration.tabExploration)
    setUnlocalizedName("projectred.exploration."+toolDef.unlocal)
    GameRegistry.register(setRegistryName(toolDef.registryName))
    setMaxDamage(toolDef.mat.getMaxUses)

    override def getCuttingStrength(item:ItemStack) = toolDef.mat.getHarvestLevel
}

class ItemGemSickle(override val toolDef:ToolDef) extends ItemTool(3, 0/*TODO This needs the correct values thrown in from the ToolDef*/,toolDef.mat, new java.util.HashSet) with TGemTool
{
    private val radiusLeaves = 1
    private val radiusCrops = 2

    override def getStrVsBlock(stack:ItemStack, state:IBlockState) =
    {
        if (state.getBlock.isInstanceOf[BlockLeaves]) efficiencyOnProperMaterial
        else super.getStrVsBlock(stack, state)
    }

    override def onBlockDestroyed(stack:ItemStack, w:World, state:IBlockState, pos:BlockPos, ent:EntityLivingBase):Boolean =
    {
        val player = ent match
        {
            case p:EntityPlayer => p
            case _ => null
        }

        if (player != null && state != null)
        {
            if (WorldLib.isLeafType(w, pos, state)) return runLeaves(stack, w, pos, player)
            else if (WorldLib.isPlantType(w, pos, state)) return runCrops(stack, w, pos, player)
        }
        super.onBlockDestroyed(stack, w, state, pos, ent)
    }

    private def runLeaves(stack:ItemStack, w:World, pos:BlockPos, player:EntityPlayer) =
    {
        var used = false
        for (i <- -radiusLeaves to radiusLeaves) for (j <- -radiusLeaves to radiusLeaves) for (k <- -radiusLeaves to radiusLeaves)
        {
            val p = pos.add(i, j, k)
            val b = w.getBlockState(p)
            if (b != null && WorldLib.isLeafType(w, pos, b))
            {
                if (b.getBlock.canHarvestBlock(w, p, player)) b.getBlock.harvestBlock(w, player, p, b, w.getTileEntity(p), stack)
                w.setBlockToAir(p)
                used = true
            }
        }

        if (used) stack.damageItem(1, player)
        used
    }

    private def runCrops(stack:ItemStack, w:World, pos:BlockPos, player:EntityPlayer) =
    {
        var used = false
        for (i <- -radiusCrops to radiusCrops) for (j <- -radiusCrops to radiusCrops)
        {
            val p = pos.add(i, 0, j)
            val b = w.getBlockState(p)
            if (b != null && WorldLib.isPlantType(w, pos, b))
            {
                if (b.getBlock.canHarvestBlock(w, p, player)) b.getBlock.harvestBlock(w, player, p, b, w.getTileEntity(p), stack)
                w.setBlockToAir(p)
                used = true
            }
        }

        if (used) stack.damageItem(1, player)
        used
    }
}


object ArmorDefs
{
    import mrtjp.projectred.ProjectRedExploration.{armorMatrialPeridot, armorMatrialRuby, armorMatrialSapphire}
    private val ruby = PartDefs.RUBY.makeStack
    private val sapphire = PartDefs.SAPPHIRE.makeStack
    private val peridot = PartDefs.PERIDOT.makeStack

    val RUBYHELMET          = ArmorDef("helmetRuby",        "ruby_helmet",          "ruby", armorMatrialRuby, ruby)
    val RUBYCHESTPLATE      = ArmorDef("chestplateRuby",    "ruby_chestplate",      "ruby", armorMatrialRuby, ruby)
    val RUBYLEGGINGS        = ArmorDef("leggingsRuby",      "ruby_leggings",        "ruby", armorMatrialRuby, ruby)
    val RUBYBOOTS           = ArmorDef("bootsRuby",         "ruby_boots",           "ruby", armorMatrialRuby, ruby)

    val SAPPHIREHELMET      = ArmorDef("helmetSapphire",    "sapphire_helmet",      "sapphire", armorMatrialSapphire, sapphire)
    val SAPPHIRECHESTPLATE  = ArmorDef("chestplateSapphire","sapphire_chestplate",  "sapphire", armorMatrialSapphire, sapphire)
    val SAPPHIRELEGGINGS    = ArmorDef("leggingsSapphire",  "sapphire_leggings",    "sapphire", armorMatrialSapphire, sapphire)
    val SAPPHIREBOOTS       = ArmorDef("bootsSapphire",     "sapphire_boots",       "sapphire", armorMatrialSapphire, sapphire)

    val PERIDOTHELMET       = ArmorDef("helmetPeridot",     "peridot_helmet",       "peridot", armorMatrialPeridot, peridot)
    val PERIDOTCHESTPLATE   = ArmorDef("chestplatePeridot", "peridot_chestplate",   "peridot", armorMatrialPeridot, peridot)
    val PERIDOTLEGGINGS     = ArmorDef("leggingsPeridot",   "peridot_leggings",     "peridot", armorMatrialPeridot, peridot)
    val PERIDOTBOOTS        = ArmorDef("bootsPeridot",      "peridot_boots",        "peridot", armorMatrialPeridot, peridot)

    case class ArmorDef(unlocal:String, registryName:String, tex:String, mat:ArmorMaterial, repair:ItemStack)
}

class ItemGemArmor(adef:ArmorDef, slot:EntityEquipmentSlot) extends ItemToolProxies.Armor(adef.mat, slot)
{
    setCreativeTab(ProjectRedExploration.tabExploration)
    setUnlocalizedName("projectred.exploration."+adef.unlocal)
    GameRegistry.register(setRegistryName(adef.registryName))

    override def getIsRepairable(ist1:ItemStack, ist2:ItemStack) =
    {
        if (adef.repair.isItemEqual(ist2)) true
        else false
    }

    override def getArmorTexture(stack: ItemStack, entity: Entity, slot: EntityEquipmentSlot, `type`: String): String = {
        import net.minecraft.inventory.EntityEquipmentSlot._
        if (slot.getSlotType == EntityEquipmentSlot.Type.ARMOR) {
            val suffix = if (slot == HEAD || slot == CHEST) 1 else 2
            return s"projectred:textures/items/world/${adef.tex}_$suffix.png"
        }
        null
    }
}

class ItemWoolGin extends ItemCraftingDamage
{
    setMaxDamage(128)
    setCreativeTab(ProjectRedExploration.tabExploration)
}

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

import com.google.common.collect.{HashMultimap, Multimap}
import net.minecraft.entity.SharedMonsterAttributes
import net.minecraft.entity.ai.attributes.AttributeModifier
import net.minecraft.entity.monster.EntityEnderman
import net.minecraft.util.DamageSource
class ItemAthame() extends ItemSword(toolMaterialDiamond)
{
    private var damage:Float = _

    setMaxDamage(100)
    setCreativeTab(ProjectRedExploration.tabExploration)

    override def getStrVsBlock(stack:ItemStack, block:IBlockState):Float = 1.0F

    override def getDamageVsEntity():Float = damage

    override def hitEntity(stack:ItemStack, entity:EntityLivingBase, player:EntityLivingBase):Boolean =
    {
        damage = toolMaterialDiamond.getDamageVsEntity

        if ((entity.isInstanceOf[EntityEnderman])) damage = 25.0F else damage = 1.0F

        val damageSource = DamageSource.causePlayerDamage(player.asInstanceOf[EntityPlayer])
        entity.attackEntityFrom(damageSource, damage)

        super.hitEntity(stack, entity, player)
    }

    override def getIsRepairable(stack:ItemStack, stack1:ItemStack):Boolean =
    {
        stack1.isItemEqual(PartDefs.SILVERINGOT.makeStack)
    }

    override def getItemEnchantability():Int = 30

    override def getItemAttributeModifiers(slot:EntityEquipmentSlot) =
    {
        val damageModifier = HashMultimap.create().asInstanceOf[Multimap[String, AttributeModifier]]
        if (slot == EntityEquipmentSlot.MAINHAND) {
            damageModifier.put(SharedMonsterAttributes.ATTACK_DAMAGE.getAttributeUnlocalizedName, new AttributeModifier(Item.ATTACK_DAMAGE_MODIFIER, "Weapon modifier", 0, 0))
        }
        damageModifier
    }
}
