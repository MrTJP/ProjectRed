package mrtjp.projectred.exploration

import java.util.{List => JList}

import codechicken.microblock.Saw
import cpw.mods.fml.common.registry.GameRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.gui.{GuiLib, Slot2, WidgetContainer}
import mrtjp.core.inventory.SimpleInventory
import mrtjp.core.item.ItemCore
import mrtjp.core.util.Enum
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExploration
import mrtjp.projectred.core.{ItemCraftingDamage, PartDefs}
import mrtjp.projectred.exploration.ToolDefs.ToolVal
import net.minecraft.block.{Block, BlockFlower, BlockLeaves}
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.EntityLivingBase
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.Item.ToolMaterial
import net.minecraft.item._
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.IIcon
import net.minecraft.world.World

class ItemBackpack extends ItemCore("projectred.exploration.backpack")
{
    setHasSubtypes(true)
    setMaxStackSize(1)
    setCreativeTab(ProjectRedExploration.tabExploration)

    override def onItemUse(stack:ItemStack, player:EntityPlayer, world:World, x:Int, y:Int, z:Int, par7:Int, par8:Float, par9:Float, par10:Float) =
    {
        openGui(player)
        true
    }

    override def onItemRightClick(stack:ItemStack, w:World, player:EntityPlayer) =
    {
        openGui(player)
        super.onItemRightClick(stack, w, player)
    }

    def openGui(player:EntityPlayer)
    {
        GuiBackpack.open(player, ItemBackpack.createContainer(player))
    }

    override def getSubItems(item:Item, tab:CreativeTabs, list:JList[_])
    {
        for (i <- 0 until 16)
            list.asInstanceOf[JList[ItemStack]].add(new ItemStack(this, 1, i))
    }

    private val icons = new Array[IIcon](16)
    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        for (i <- 0 until 16)
            icons(i) = reg.registerIcon("projectred:backpacks/"+i)
    }

    override def getIconFromDamage(meta:Int) = icons(meta)
}

object ItemBackpack
{
    val oreDictionaryVal = "prbackpack"

    def getInv(player:EntityPlayer) =
    {
        val held = player.getHeldItem
        if (held != null && held.getItem == ProjectRedExploration.itemBackpack)
            new BagInventory(player, held)
        else new BagInventory(player, new ItemStack(ProjectRedExploration.itemBackpack))
    }

    def createContainer(player:EntityPlayer) =
    {
        val cont = new WidgetContainer
        {
            override def +(s:Slot2):this.type =
            {
                if (s.getSlotIndex == player.inventory.currentItem && s.inventory == player.inventory)
                    s.setRemove(false)
                super.+(s)
            }
        }

        val inv = getInv(player)

        var slot = 0
        for ((x, y) <- GuiLib.createSlotGrid(8, 18, 9, 3, 0, 0))
        {
            cont + new Slot2(inv, slot, x, y)
            slot += 1
        }

        cont.addPlayerInv(player, 8, 86)

        cont
    }
}

class BagInventory(player:EntityPlayer, bag:ItemStack) extends SimpleInventory(27, 64)
{
    loadInventory()

    private def loadInventory()
    {
        assertNBT(bag)
        load(bag.getTagCompound.getCompoundTag("baginv"))
    }

    private def saveInventory()
    {
        val nbt = new NBTTagCompound
        save(nbt)
        assertNBT(bag)
        bag.getTagCompound.setTag("baginv", nbt)
        refreshNBT()
    }

    private def refreshNBT()
    {
        val currentBag = player.getHeldItem
        if (currentBag != null && currentBag.getItem == ProjectRedExploration.itemBackpack)
            currentBag.setTagCompound(bag.getTagCompound)
    }

    override def markDirty()
    {
        super.markDirty()
        saveInventory()
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

    private def assertNBT(s:ItemStack)
    {
        if (!s.hasTagCompound) s.setTagCompound(new NBTTagCompound)
    }
}

object ToolDefs extends Enum
{
    type EnumVal = ToolVal

    private val wood = new ItemStack(Blocks.planks)
    private val flint = new ItemStack(Items.flint)
    private val iron = new ItemStack(Items.iron_ingot)
    private val gold = new ItemStack(Items.gold_ingot)
    private val ruby = PartDefs.RUBY.makeStack
    private val sapphire = PartDefs.SAPPHIRE.makeStack
    private val peridot = PartDefs.PERIDOT.makeStack
    private val diamond = new ItemStack(Items.diamond)

    import mrtjp.projectred.ProjectRedExploration.{toolMaterialPeridot, toolMaterialRuby, toolMaterialSapphire}
    import net.minecraft.item.Item.ToolMaterial.{EMERALD => toolMaterialEmerald, GOLD => toolMaterialGold, IRON => toolMaterialIron, STONE => toolMaterialStone, WOOD => toolMaterialWood}

    val RUBYAXE = ToolDefs("axeruby", toolMaterialRuby, ruby)
    val SAPPHIREAXE = ToolDefs("axesapphire", toolMaterialSapphire, sapphire)
    val PERIDOTAXE = ToolDefs("axeperidot", toolMaterialPeridot, peridot)

    val RUBYPICKAXE = ToolDefs("pickaxeruby", toolMaterialRuby, ruby)
    val SAPPHIREPICKAXE = ToolDefs("pickaxesapphire", toolMaterialSapphire, sapphire)
    val PERIDOTPICKAXE = ToolDefs("pickaxeperidot", toolMaterialPeridot, peridot)

    val RUBYSHOVEL = ToolDefs("shovelruby", toolMaterialRuby, ruby)
    val SAPPHIRESHOVEL = ToolDefs("shovelsapphire", toolMaterialSapphire, sapphire)
    val PERIDOTSHOVEL = ToolDefs("shovelperidot", toolMaterialPeridot, peridot)

    val RUBYSWORD = ToolDefs("swordruby", toolMaterialRuby, ruby)
    val SAPPHIRESWORD = ToolDefs("swordsapphire", toolMaterialSapphire, sapphire)
    val PERIDOTSWORD = ToolDefs("swordperidot", toolMaterialPeridot, peridot)

    val RUBYHOE = ToolDefs("hoeruby", toolMaterialRuby, ruby)
    val SAPPHIREHOE = ToolDefs("hoesapphire", toolMaterialSapphire, sapphire)
    val PERIDOTHOE = ToolDefs("hoeperidot", toolMaterialPeridot, peridot)

    val WOODSAW = ToolDefs("sawwood", toolMaterialWood, wood)
    val STONESAW = ToolDefs("sawstone", toolMaterialStone, flint)
    val IRONSAW = ToolDefs("sawiron", toolMaterialIron, iron)
    val GOLDSAW = ToolDefs("sawgold", toolMaterialGold, gold)
    val RUBYSAW = ToolDefs("sawruby", toolMaterialRuby, ruby)
    val SAPPHIRESAW = ToolDefs("sawsapphire", toolMaterialSapphire, sapphire)
    val PERIDOTSAW = ToolDefs("sawperidot", toolMaterialPeridot, peridot)
    val DIAMONDSAW = ToolDefs("sawdiamond", toolMaterialEmerald, diamond)

    val WOODSICKLE = ToolDefs("sicklewood", toolMaterialWood, wood)
    val STONESICKLE = ToolDefs("sicklestone", toolMaterialStone, flint)
    val IRONSICKLE = ToolDefs("sickleiron", toolMaterialIron, iron)
    val GOLDSICKLE = ToolDefs("sicklegold", toolMaterialGold, gold)
    val RUBYSICKLE = ToolDefs("sickleruby", toolMaterialRuby, ruby)
    val SAPPHIRESICKLE = ToolDefs("sicklesapphire", toolMaterialSapphire, sapphire)
    val PERIDOTSICKLE = ToolDefs("sickleperidot", toolMaterialPeridot, peridot)
    val DIAMONDSICKLE = ToolDefs("sicklediamond", toolMaterialEmerald, diamond)

    def apply(s:String, m:ToolMaterial, r:ItemStack) = new ToolVal(s, m, r)
    class ToolVal(val unlocal:String, val mat:ToolMaterial, val repair:ItemStack) extends Value
    {
        override def name = unlocal
    }
}

trait TGemTool extends Item
{
    setUnlocalizedName("projectred.exploration."+tool.unlocal)
    setTextureName("projectred:gemtools/"+tool.unlocal)
    setCreativeTab(ProjectRedExploration.tabExploration)
    GameRegistry.registerItem(this, "projectred.exploration."+tool.unlocal)

    def tool:ToolVal

    override def getIsRepairable(ist1:ItemStack, ist2:ItemStack) =
    {
        if (tool.repair.isItemEqual(ist2)) true
        else super.getIsRepairable(ist1, ist2)
    }
}

// Weird scala compiler issue. These classes are implemented in java.
class ItemGemAxe(override val tool:ToolVal) extends ItemAxe(tool.mat) with TGemTool
class ItemGemPickaxe(override val tool:ToolVal) extends ItemPickaxe(tool.mat) with TGemTool
class ItemGemShovel(override val tool:ToolVal) extends ItemSpade(tool.mat) with TGemTool
class ItemGemSword(override val tool:ToolVal) extends ItemSword(tool.mat) with TGemTool
class ItemGemHoe(override val tool:ToolVal) extends ItemHoe(tool.mat) with TGemTool

class ItemGemSaw(val tool:ToolVal) extends ItemCraftingDamage("projectred.exploration."+tool.unlocal) with Saw
{
    setMaxDamage(tool.mat.getMaxUses)
    setCreativeTab(ProjectRedExploration.tabExploration)

    override def getCuttingStrength(item:ItemStack) = tool.mat.getHarvestLevel
    override def registerIcons(reg:IIconRegister){}
}

class ItemGemSickle(override val tool:ToolVal) extends ItemTool(3, tool.mat, new java.util.HashSet) with TGemTool
{
    private val radiusLeaves = 1
    private val radiusCrops = 2

    override def func_150893_a(stack:ItemStack, b:Block) =
    {
        if (b.isInstanceOf[BlockLeaves]) efficiencyOnProperMaterial
        else super.func_150893_a(stack, b)
    }

    override def onBlockDestroyed(stack:ItemStack, w:World, b:Block, x:Int, y:Int, z:Int, ent:EntityLivingBase):Boolean =
    {
        val player = ent match
        {
            case p:EntityPlayer => p
            case _ => null
        }

        if (player != null && b != null)
        {
            if (b.isLeaves(w, x, y, z)) return runLeaves(stack, w, x, y, z, player)
            else if (b.isInstanceOf[BlockFlower]) return runCrops(stack, w, x, y, z, player)
        }
        super.onBlockDestroyed(stack, w, b, x, y, z, ent)
    }

    private def runLeaves(stack:ItemStack, w:World, x:Int, y:Int, z:Int, player:EntityPlayer) =
    {
        var used = false
        for (i <- -radiusLeaves to radiusLeaves) for (j <- -radiusLeaves to radiusLeaves) for (k <- -radiusLeaves to radiusLeaves)
        {
            val lx = x+i
            val ly = y+j
            val lz = z+k
            val b = w.getBlock(lx, ly, lz)
            val meta = w.getBlockMetadata(lx, ly, lz)
            if (b != null && WorldLib.isLeafType(w, x, y, z, b))
            {
                if (b.canHarvestBlock(player, meta)) b.harvestBlock(w, player, lx, ly, lz, meta)
                w.setBlockToAir(lx, ly, lz)
                used = true
            }
        }

        if (used) stack.damageItem(1, player)
        used
    }

    private def runCrops(stack:ItemStack, w:World, x:Int, y:Int, z:Int, player:EntityPlayer) =
    {
        var used = false
        for (i <- -radiusCrops to radiusCrops) for (j <- -radiusCrops to radiusCrops)
        {
            val lx = x+i
            val ly = y
            val lz = z+j
            val b = w.getBlock(lx, ly, lz)
            val meta = w.getBlockMetadata(lx, ly, lz)
            if (b != null && WorldLib.isPlantType(w, x, y, z, b))
            {
                if (b.canHarvestBlock(player, meta)) b.harvestBlock(w, player, lx, ly, lz, meta)
                w.setBlockToAir(lx, ly, lz)
                used = true
            }
        }

        if (used) stack.damageItem(1, player)
        used
    }
}

class ItemWoolGin extends ItemCraftingDamage("projectred.exploration.woolgin")
{
    setMaxDamage(128)
    setCreativeTab(ProjectRedExploration.tabExploration)

    override def registerIcons(reg:IIconRegister)
    {
        itemIcon = reg.registerIcon("projectred:woolgin")
    }
}