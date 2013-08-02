package mrtjp.projectred.items;

import mrtjp.projectred.crafting.ProjectRedTabs;
import net.minecraft.block.Block;
import net.minecraft.block.BlockMelon;
import net.minecraft.block.BlockPumpkin;
import net.minecraft.block.BlockSapling;
import net.minecraft.block.BlockStem;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.minecraftforge.common.IPlantable;
import net.minecraftforge.common.IShearable;

/**
 * The sickle is a tool that allows the player to harvest leaves and crops in a
 * 3x3x3 area around the block where the player destroyed
 *
 * @author Shirkit
 */
public class ItemSickle extends Item {

    public static final int radiusElse = 2;
    public static final int radiusLeaves = 1;

    public enum EnumSickle {

        WOOD(64, "sickleWood", "Wood Sickle", Block.planks),
        STONE(128, "sickleStone", "Stone Sickle", Block.stone),
        IRON(384, "sickleIron", "Iron Sickle", Item.ingotIron),
        GOLD(768, "SickleGold", "Gold Sickle", Item.ingotGold),
        DIAMOND(1536, "sickleDiamond", " Diamond Sickle", Item.diamond);
        //
        public final int id = this.ordinal() - 1;
        public final String fullName, ulocalName;
        public final int maxDamage;
        public final Object material;

        private EnumSickle(int maxDamage, String ulocalname, String name, Object material) {
            this.fullName = name;
            this.ulocalName = ulocalname;
            this.maxDamage = maxDamage;
            this.material = material;
        }
    }
    public EnumSickle type;

    public ItemSickle(int id, EnumSickle type) {
        super(id);
        setMaxDamage(type.maxDamage);
        setMaxStackSize(1);
        setCreativeTab(ProjectRedTabs.tabTools);
        setUnlocalizedName("projectred.items.".concat(type.ulocalName));
        this.type = type;
    }

    @Override
    public boolean onBlockDestroyed(ItemStack stack, World world, int blockId, int blockX, int blockY, int blockZ, EntityLivingBase entity) {
        boolean work = false;

        if (!(entity instanceof EntityPlayer))
            return false;

        EntityPlayer player = (EntityPlayer) entity;

        Block source = Block.blocksList[blockId];
        if (!world.isRemote && canHit(blockId)) {

            if (source.isLeaves(world, blockX, blockY, blockZ))
                for (int x = -radiusLeaves; x <= radiusLeaves; x++)
                    for (int y = -radiusLeaves; y <= radiusLeaves; y++)
                        for (int z = -radiusLeaves; z <= radiusLeaves; z++) {
                            int id = world.getBlockId(blockX + x, blockY + y, blockZ + z);
                            if (canHit(id)) {
                                Block kind = Block.blocksList[id];
                                if (kind.isLeaves(world, blockX + x, blockY + y, blockZ + z)) {
                                    int meta = world.getBlockMetadata(blockX + x, blockY + y, blockZ + z);
                                    if (kind.canHarvestBlock(player, meta)) {
                                        // We set to AIR to prevent all the cut sounds from playing (instead of destroying the block)
                                        kind.harvestBlock(world, player, blockX + x, blockY + y, blockZ + z, meta);
                                        world.setBlock(blockX + x, blockY + y, blockZ + z, 0);
                                    }
                                    work = true;
                                }
                            }
                        }
            else
                for (int x = -radiusElse; x <= radiusElse; x++)
                    for (int z = -radiusElse; z <= radiusElse; z++) {
                        int id = world.getBlockId(blockX + x, blockY, blockZ + z);
                        if (canHit(id)) {
                            Block kind = Block.blocksList[id];
                            int meta = world.getBlockMetadata(blockX + x, blockY, blockZ + z);
                            if (kind.canHarvestBlock(player, meta)) {
                                // We set to AIR to prevent all the cut sounds from playing (instead of destroying the block)
                                kind.harvestBlock(world, player, blockX + x, blockY, blockZ + z, meta);
                                world.setBlock(blockX + x, blockY, blockZ + z, 0);
                            }
                            work = true;
                        }
                    }

            if (work)
                stack.damageItem(1, entity);
        }
        return work;
    }

    @Override
    public boolean canHarvestBlock(Block par1Block) {
        return canHit(par1Block.blockID);
    }

    private boolean canHit(int id) {
        Block b = Block.blocksList[id];
        return (!(b instanceof BlockSapling) && !(b instanceof BlockStem)) && (b instanceof IShearable || b instanceof IPlantable || b instanceof BlockPumpkin || b instanceof BlockMelon);
    }

    @Override
    public float getStrVsBlock(ItemStack par1ItemStack, Block par2Block) {
        if (canHarvestBlock(par2Block)) {
            return 3f;
        }
        return super.getStrVsBlock(par1ItemStack, par2Block);
    }

    @Override
    public void registerIcons(IconRegister par1IconRegister) {
        this.itemIcon = par1IconRegister.registerIcon("projectred:".concat(type.ulocalName));
    }
}