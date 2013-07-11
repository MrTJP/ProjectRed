package mrtjp.projectred.tiles;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.crafting.AlloySmelterRecipe;
import mrtjp.projectred.crafting.CraftingRecipeManager;
import mrtjp.projectred.interfaces.IGuiOpenControler;
import mrtjp.projectred.interfaces.ISimpleInventoryListener;
import mrtjp.projectred.network.GuiIDs;
import mrtjp.projectred.network.PacketHandler;
import mrtjp.projectred.network.packets.AlloySmelterInitPacket;
import mrtjp.projectred.network.packets.AlloySmelterUpdatePacket;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.SimpleInventory;
import mrtjp.projectred.utils.gui.GhostContainer;
import mrtjp.projectred.utils.gui.RestrictedSlot.ISlotCheck;
import net.minecraft.block.Block;
import net.minecraft.block.material.Material;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.Item;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemHoe;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemSword;
import net.minecraft.item.ItemTool;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.packet.Packet;
import net.minecraftforge.common.ForgeDirection;
import cpw.mods.fml.common.registry.GameRegistry;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class TileAlloySmelter extends TileMachineBase implements IInventory, IGuiOpenControler {

	private SimpleInventory _inv = new SimpleInventory(11, "alloy", 64);
	private List<EntityPlayer> _watchers = new ArrayList<EntityPlayer>();

	// True when working on something
	public boolean hasWork = false;

	// Between 0 and whatever the recipe requires
	public int progress = 0;

	// Between 0 and 6400
	public int heat = 0;

	// Next tick, all near by will be notified.
	public boolean queueWatcherUpdate = false;
	// Next tick, work will be rechecked.
	public boolean queueWorkUpdate = false;
	// Cahce, used to avoid checking all recipes every tick.
	public int burnTimeForRecipe = 0;
	public TileAlloySmelter() {
	}

	@Override
	public void onBlockBreak() {
		_inv.dropContents(worldObj, xCoord, yCoord, zCoord);
	}

	public Container getContainer(EntityPlayer player) {
		GhostContainer ghost = new GhostContainer(player, this, this);
		// Crafting matrix
		for (int i = 0; i < 3; i++) {
			for (int j = 0; j < 3; j++) {
				final int slotNumber = i * 3 + j;
				ghost.addNormalSlot(slotNumber, this, 44 + (j * 18), 17 + (i * 18));
			}
		}
		// Fuel slot
		ghost.addRestrictedSlot(9, this, 107, 17, new ISlotCheck() {
			@Override
			public boolean isSlotAllowed(ItemStack stack) {
				return getItemBurnTime(stack) > 0;
			}
		});
		// Result slot
		ghost.addRestrictedSlot(10, this, 141, 47, -1);
		// Player inv slots
		ghost.addNormalSlotsForPlayerInventory(8, 84);
		return ghost;
	}

	@Override
	public void onBlockClicked(EntityPlayer player) {

	}

	@Override
	public boolean onBlockActivated(EntityPlayer player) {
		if (!player.isSneaking()) {
			player.openGui(ProjectRed.instance, GuiIDs.ID_Alloy, player.worldObj, xCoord, yCoord, zCoord);
			return true;
		}
		return false;
	}

	@Override
	public void writeToNBT(NBTTagCompound nbt) {
		super.writeToNBT(nbt);
		_inv.writeToNBT(nbt);
		nbt.setInteger("heat", heat);
		nbt.setInteger("progress", progress);
		nbt.setBoolean("work", hasWork);
	}

	@Override
	public void readFromNBT(NBTTagCompound nbt) {
		super.readFromNBT(nbt);
		_inv.readFromNBT(nbt);
		heat = nbt.getInteger("heat");
		progress = nbt.getInteger("progress");
		hasWork = nbt.getBoolean("work");
		int index = nbt.getInteger("recipeIndex");
		updateNextTick = true;
	}

	@Override
	public Packet getDescriptionPacket() {
		AlloySmelterInitPacket packet = PacketHandler.getPacket(AlloySmelterInitPacket.class);
		packet.posX = xCoord;
		packet.posY = yCoord;
		packet.posZ = zCoord;
		NBTTagCompound nbt = new NBTTagCompound();
		writeToNBT(nbt);
		packet.tiledata = nbt;
		return packet.getPacket();
	}

	@Override
	public int getSizeInventory() {
		return _inv.getSizeInventory();
	}

	@Override
	public ItemStack getStackInSlot(int i) {
		return _inv.getStackInSlot(i);
	}

	@Override
	public ItemStack decrStackSize(int i, int j) {
		ItemStack stack = _inv.decrStackSize(i, j);
		return stack;
	}

	@Override
	public ItemStack getStackInSlotOnClosing(int i) {
		return _inv.getStackInSlotOnClosing(i);
	}

	@Override
	public void setInventorySlotContents(int i, ItemStack itemstack) {
		_inv.setInventorySlotContents(i, itemstack);
	}

	@Override
	public String getInvName() {
		return _inv.getInvName();
	}

	@Override
	public boolean isInvNameLocalized() {
		return true;
	}

	@Override
	public int getInventoryStackLimit() {
		return _inv.getInventoryStackLimit();
	}

	@Override
	public boolean isUseableByPlayer(EntityPlayer player) {
		return _inv.isUseableByPlayer(player);
	}

	@Override
	public void openChest() {
		_inv.openChest();
	}

	@Override
	public void closeChest() {
		_inv.closeChest();
	}

	@Override
	public boolean isStackValidForSlot(int i, ItemStack itemstack) {
		return true;
	}

	@Override
	public void onGuiOpenedBy(EntityPlayer player) {
		_watchers.add(player);
	}

	@Override
	public void onGuiClosedBy(EntityPlayer player) {
		_watchers.remove(player);
	}

	@Override
	public boolean shouldUseSpecialTextureForSide(int side) {
		if (heat > 0 && ForgeDirection.getOrientation(side) == ForgeDirection.getOrientation(rotation).getOpposite()) {
			return true;
		} else {
			return false;
		}
	}

	@Override
	public int getLightLevel() {
		return heat > 0 ? (hasWork ? 13 : 10) : 0;
	}

	@Override
	public void updateEntity() {
		if (queueWorkUpdate) {
			queueWorkUpdate = false;
			hasWork = hasWork();
		}
		if (hasWork && heat <= 0) {
			eatFuel();
			if (heat > 0) {
				queueWatcherUpdate = true;
				updateNextTick = true;
			}
		}
		if (heat > 0) {
			if (BasicUtils.isClient(worldObj)) {
				spawnParticles();
			}
			if (hasWork) {
				progress++;
			}
			if (heat > 0) {
				heat--;
			}
			if (heat <= 0) {
				updateNextTick = true;
			}
			queueWatcherUpdate = true;
		}
		if (hasWork) {
			if (progress >= burnTimeForRecipe) {
				AlloySmelterRecipe r = getSuggestedRecipe();
				if (r != null) {
					eatAllResourcesForRecipe(r);
					ItemStack result = r.getResult();
					r._handler.onItemCrafted(result);
					if (_inv.getStackInSlot(10) == null) {
						_inv.setInventorySlotContents(10, result);
					} else {
						ItemStack inslot = _inv.getStackInSlot(10);
						inslot.stackSize += result.stackSize;
						_inv.setInventorySlotContents(10, inslot);
					}
					progress = 0;
					burnTimeForRecipe = 0;
					queueWatcherUpdate = true;
					queueWorkUpdate = true;
					updateNextTick = true;
				}
			}
		}
		if (queueWatcherUpdate) {

			queueWatcherUpdate = false;
			updateWatchers();
		}
		if (updateNextTick) {
			// All nearby players need to be updated if the status of work
			// changes, or if heat runs out / starts up, in order to change
			// texture.
			updateNextTick = false;
			worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
			worldObj.updateAllLightTypes(xCoord, yCoord, zCoord);
		}
	}

	@SideOnly(Side.CLIENT)
	private void spawnParticles() {
		Random ran = new Random();
		if (hasWork) {
			worldObj.spawnParticle("smoke", (double) xCoord + .35f + ran.nextFloat() * 4.5F / 16.0F, (double) yCoord + 1, (double) zCoord + .35f + ran.nextFloat() * 4.5F / 16.0F, 0.0D, 0.0D, 0.0D);
			worldObj.spawnParticle("flame", (double) xCoord + .35f + ran.nextFloat() * 4.5F / 16.0F, (double) yCoord + 1, (double) zCoord + .35f + ran.nextFloat() * 4.5F / 16.0F, 0.0D, 0.0D, 0.0D);
		}
		if (ran.nextFloat() > (hasWork ? .45f : .1f)) {
			return;
		}
		int l = ForgeDirection.getOrientation(rotation).getOpposite().ordinal();
		float f = (float) xCoord + 0.5F;
		float f1 = (float) yCoord + 0.0F + ran.nextFloat() * 6.0F / 16.0F;
		float f2 = (float) zCoord + 0.5F;
		float f3 = 0.52F;
		float f4 = ran.nextFloat() * 0.6F - 0.3F;

		if (l == 4) {
			worldObj.spawnParticle("smoke", (double) (f - f3), (double) f1, (double) (f2 + f4), 0.0D, 0.0D, 0.0D);
			worldObj.spawnParticle("flame", (double) (f - f3), (double) f1, (double) (f2 + f4), 0.0D, 0.0D, 0.0D);
		} else if (l == 5) {
			worldObj.spawnParticle("smoke", (double) (f + f3), (double) f1, (double) (f2 + f4), 0.0D, 0.0D, 0.0D);
			worldObj.spawnParticle("flame", (double) (f + f3), (double) f1, (double) (f2 + f4), 0.0D, 0.0D, 0.0D);
		} else if (l == 2) {
			worldObj.spawnParticle("smoke", (double) (f + f4), (double) f1, (double) (f2 - f3), 0.0D, 0.0D, 0.0D);
			worldObj.spawnParticle("flame", (double) (f + f4), (double) f1, (double) (f2 - f3), 0.0D, 0.0D, 0.0D);
		} else if (l == 3) {
			worldObj.spawnParticle("smoke", (double) (f + f4), (double) f1, (double) (f2 + f3), 0.0D, 0.0D, 0.0D);
			worldObj.spawnParticle("flame", (double) (f + f4), (double) f1, (double) (f2 + f3), 0.0D, 0.0D, 0.0D);
		}

	}

	public void updateWatchers() {
		AlloySmelterUpdatePacket packet = PacketHandler.getPacket(AlloySmelterUpdatePacket.class);
		packet.posX = xCoord;
		packet.posY = yCoord;
		packet.posZ = zCoord;
		packet.heat = this.heat;
		packet.progress = this.progress;
		BasicUtils.sendPacketToPlayerList(packet.getPacket(), _watchers);
	}

	public boolean hasWork() {
		AlloySmelterRecipe r =  getSuggestedRecipe();
		boolean shouldWork = (r != null && (_inv.getStackInSlot(10) == null || (BasicUtils.areStacksTheSame(_inv.getStackInSlot(10), r.getResult()) && _inv.getStackInSlot(10).stackSize + r.getResult().stackSize <= _inv.getInventoryStackLimit())));		
		if (shouldWork) {
			burnTimeForRecipe = r.getBurnTime();
			updateNextTick = true;
			return true;
		}
		progress = 0;
		burnTimeForRecipe = 0;
		return false;
	}

	private void eatFuel() {
		int burntime = getItemBurnTime(_inv.getStackInSlot(9));
		if (burntime > 0) {
			_inv.decrStackSize(9, 1);
			heat += burntime;
			if (heat > 6400) {
				heat = 6400;
			}
		}
	}

	private void eatResource(ItemStack s) {
		int missing = s.stackSize;
		while (missing > 0) {
			for (int i = 0; i < 9; i++) {
				ItemStack stackinslot = _inv.getStackInSlot(i);
				if (stackinslot == null) {
					continue;
				}
				if (BasicUtils.areStacksTheSame(stackinslot, s)) {
					_inv.decrStackSize(i, 1);
					missing--;
					if (missing <= 0) {
						break;
					}
				}
			}
		}
	}

	private void eatAllResourcesForRecipe(AlloySmelterRecipe r) {
		for (ItemStack s : r.getMatrix()) {
			eatResource(s);
		}
	}

	public AlloySmelterRecipe getSuggestedRecipe() {
		for (AlloySmelterRecipe r : AlloySmelterRecipe.getAlloyRecipes()) {
			if (r.calculateMatch(getCraftingMatrix())) {
				return r;
			}
		}
		return null;
	}

	private ItemStack[] getCraftingMatrix() {
		ItemStack[] matrix = new ItemStack[9];
		for (int i = 0; i < 9; i++) {
			ItemStack inslot = _inv.getStackInSlot(i);
			if (inslot != null) {
				matrix[i] = inslot.copy();
			} else {
				matrix[i] = null;
			}
		}
		return matrix;
	}

	private int getItemBurnTime(ItemStack stack) {
		if (stack == null) {
			return 0;
		} else {
			int i = stack.getItem().itemID;
			Item item = stack.getItem();
			if (stack.getItem() instanceof ItemBlock && Block.blocksList[i] != null) {
				Block block = Block.blocksList[i];
				if (block == Block.woodSingleSlab) {
					return 150;
				}
				if (block.blockMaterial == Material.wood) {
					return 300;
				}
			}
			if (item instanceof ItemTool && ((ItemTool) item).getToolMaterialName().equals("WOOD"))
				return 200;
			if (item instanceof ItemSword && ((ItemSword) item).getToolMaterialName().equals("WOOD"))
				return 200;
			if (item instanceof ItemHoe && ((ItemHoe) item).getMaterialName().equals("WOOD"))
				return 200;
			if (i == Item.stick.itemID)
				return 100;
			if (i == Item.coal.itemID)
				return 1600;
			if (i == Item.bucketLava.itemID)
				return 20000;
			if (i == Block.sapling.blockID)
				return 100;
			if (i == Item.blazeRod.itemID)
				return 2400;
			return GameRegistry.getFuelValue(stack);
		}
	}

	@Override
	public void onInventoryChanged() {
		queueWorkUpdate = true;
	}
}
