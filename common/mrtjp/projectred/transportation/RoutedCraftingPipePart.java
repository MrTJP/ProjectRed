package mrtjp.projectred.transportation;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.DelayQueue;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.inventory.GhostContainer2;
import mrtjp.projectred.core.inventory.GhostContainer2.SlotExtended;
import mrtjp.projectred.core.inventory.InventoryWrapper;
import mrtjp.projectred.core.inventory.SimpleInventory;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.core.utils.Pair2;
import mrtjp.projectred.core.utils.PostponedWorkItem;
import mrtjp.projectred.transportation.RequestBranchNode.CraftingPromise;
import mrtjp.projectred.transportation.RequestBranchNode.DeliveryPromise;
import mrtjp.projectred.transportation.RequestBranchNode.ExcessPromise;
import mrtjp.projectred.transportation.RoutedPayload.SendPriority;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MovingObjectPosition;
import codechicken.core.IGuiPacketSender;
import codechicken.core.ServerUtils;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.packet.PacketCustom;

public class RoutedCraftingPipePart extends RoutedPipePart_InvConnect implements IWorldCrafter {

    private SimpleInventory matrix = new SimpleInventory(10, "matrix", 256);
    private DeliveryManager manager = new DeliveryManager();
    
    private final LinkedList<Pair2<ItemKeyStack, IWorldRequester>> excess = new LinkedList<Pair2<ItemKeyStack, IWorldRequester>>();
    private final DelayQueue<PostponedWorkItem<ItemKeyStack>> lost = new DelayQueue<PostponedWorkItem<ItemKeyStack>>();
    
    public int priority = 0;

    private int remainingDelay = operationDelay();
    private int remainindDelay2 = operationDelay2();
    /**
     *  Standard operation delay
     */
    private int operationDelay() {
        return 10;
    }
    /**
     *  Lost items handling delay
     */
    private int operationDelay2() {
        return 40;
    }

    protected int itemsToExtract() {
        return 1;
    }

    protected int stacksToExtract() {
        return 1;
    }

    @Override
    public void read(MCDataInput packet, int switch_key) {
        if (switch_key == 45)
            priority = packet.readInt();
        else
            super.read(packet, switch_key);
    }

    public void priorityUp() {
        int old = priority;
        priority = Math.min(100, priority+1);
        if (old != priority)
            sendPriorityUpdate();
    }

    public void priorityDown() {
        int old = priority;
        priority = Math.max(-100, priority-1);
        if (old != priority)
            sendPriorityUpdate();
    }

    private void sendPriorityUpdate() {
        if (!world().isRemote)
            tile().getWriteStream(this).writeByte(45).writeInt(priority);
    }

    @Override
    public void update() {
        super.update();
        if (!world().isRemote) {
            if (--remainingDelay <= 0) {
                remainingDelay = operationDelay();
                operationTick();
            }
            
            if (--remainindDelay2 <= 0) {
                remainindDelay2 = operationDelay2();
                lostHandleTick();
            }
        }
    }

    private void operationTick() {
        IInventory real = getInventory();
        if (real == null) {
            if (manager.hasOrders())
                manager.dispatchFailed();
            else
                excess.clear();
            return;
        }
        int side = getInterfacedSide();

        InventoryWrapper inv = InventoryWrapper.wrapInventory(real).setSlotsFromSide(side);

        List<ItemKeyStack> wanted = getCraftedItems();
        if (wanted == null || wanted.isEmpty())
            return;

        int itemsleft = itemsToExtract();
        int stacksleft = stacksToExtract();

        while (itemsleft > 0 && stacksleft > 0 && (manager.hasOrders() || !excess.isEmpty())) {
            Pair2<ItemKeyStack, IWorldRequester> nextOrder;
            boolean processingOrder=false;
            if (manager.hasOrders()) {
                nextOrder = manager.peek();
                processingOrder = true;
            } else
                nextOrder = excess.getFirst();

            ItemKeyStack keyStack = nextOrder.getValue1();

            int maxToSend = Math.min(itemsleft, keyStack.stackSize);
            maxToSend = Math.min(keyStack.key().getStackLimit(), maxToSend);
            int available = inv.extractItem(keyStack.key(), maxToSend);

            if (available <= 0)
                break;

            ItemKey key = keyStack.key();
            while (available > 0) {
                int numToSend = Math.min(available, key.getStackLimit());
                numToSend = Math.min(numToSend, keyStack.stackSize);
                if (numToSend == 0)
                    break;
                stacksleft -= 1;
                itemsleft -= numToSend;
                available -= numToSend;
                ItemStack toSend = key.makeStack(numToSend);

                if (processingOrder) {
                    queueStackToSend(toSend, side, SendPriority.ACTIVE, nextOrder.getValue2().getRouter().getIPAddress());
                    manager.dispatchSuccessful(numToSend, false);

                    if (manager.hasOrders())
                        nextOrder = manager.peek();
                    else {
                        processingOrder = false;
                        if (!excess.isEmpty())
                            nextOrder = excess.getFirst();
                    }
                } else {
                    removeExcess(key, numToSend);
                    queueStackToSend(toSend, side, SendPriority.WANDERING, -1);
                }
            }
        }
    }
    
    private void lostHandleTick() {
        if (lost.isEmpty())
            return;
                
        PostponedWorkItem<ItemKeyStack> post = null;
        
        while ((post = lost.poll()) != null) {
            ItemKeyStack stack = post.getItem();
            int toRequest = stack.stackSize;
            toRequest = Math.min(toRequest, getActiveFreeSpace(stack.key()));
            
            if (toRequest <= 0) {
                lost.add(new PostponedWorkItem<ItemKeyStack>(stack, 5000));
                continue;
            }
            
            RequestConsole req = new RequestConsole().setDestination(this);
            req.setPulling(true).setCrafting(true).setPartials(true);
            
            int requested = req.makeRequest(ItemKeyStack.get(stack.key(), toRequest)).requested();
            
            if (requested < stack.stackSize) {
                stack.stackSize -= requested;
                lost.add(new PostponedWorkItem<ItemKeyStack>(stack, 5000));
            }
        }
    }
    
    @Override
    public void trackedItemLost(ItemKeyStack s) {
        lost.add(new PostponedWorkItem<ItemKeyStack>(s, 5000));
    }

    private void removeExcess(ItemKey item, int amount) {
        Iterator<Pair2<ItemKeyStack, IWorldRequester>> iter = excess.iterator();
        while (iter.hasNext()) {
            ItemKeyStack stack = iter.next().getValue1();
            if (stack.key().equals(item))
                if (amount > stack.stackSize) {
                    amount -= stack.stackSize;
                    iter.remove();
                    if (amount <= 0)
                        return;
                } else {
                    stack.stackSize -= amount;
                    break;
                }
        }
    }

    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack item) {
        if (super.activate(player, hit, item))
            return true;

        openGui(player);
        return true;
    }

    public void openGui(EntityPlayer player) {
        if (world().isRemote) return;

        ServerUtils.openSMPContainer((EntityPlayerMP) player, createContainer(player), new IGuiPacketSender() {
            @Override
            public void sendPacket(EntityPlayerMP player, int windowId) {
                PacketCustom p = new PacketCustom(TransportationSPH.channel, NetConstants.gui_CraftingPipe_open);
                p.writeCoord(x(), y(), z());
                p.writeByte(windowId);
                p.sendToPlayer(player);
                p.writeInt(priority);
            }
        });
    }

    public Container createContainer(EntityPlayer player) {
        GhostContainer2 ghost = new GhostContainer2(player.inventory);
        int slot = 0;
        for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(26, 26, 3, 3, 0, 0))
            ghost.addCustomSlot(new SlotExtended(matrix, slot++, p.getValue1(), p.getValue2()).setGhosting(true));

        ghost.addCustomSlot(new SlotExtended(matrix, slot++, 117, 63).setGhosting(true));

        ghost.addPlayerInventory(8, 118);
        return ghost;
    }

    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        matrix.save(tag);
    }

    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        matrix.load(tag);
    }


    @Override
    public void requestPromises(RequestBranchNode request, int existingPromises) {
        if (excess.isEmpty()) return;

        ItemKey requestedItem = request.getRequestedPackage();
        List<ItemKeyStack> providedItem = getCraftedItems();
        for (ItemKeyStack item : providedItem)
            if (item.key() == requestedItem)
                return;

        if (!providedItem.contains(requestedItem))
            return;

        int remaining = 0;
        for (Pair2<ItemKeyStack, IWorldRequester> extra : excess)
            if (extra.getValue1().key() == requestedItem)
                remaining += extra.getValue1().stackSize;

        remaining -= existingPromises;
        if (remaining <= 0)
            return;

        ExcessPromise promise = new ExcessPromise();
        promise.setUsed(true).setPackage(requestedItem)
        .setSize(Math.min(remaining, request.getMissingCount())).setSender(this);

        request.addPromise(promise);
    }

    @Override
    public void deliverPromises(DeliveryPromise promise, IWorldRequester requester) {
        if (promise instanceof ExcessPromise)
            removeExcess(promise.thePackage, promise.size);

        manager.addOrder(ItemKeyStack.get(promise.thePackage, promise.size), requester);
    }

    @Override
    public void getBroadcastedItems(Map<ItemKey, Integer> map) {
    }

    @Override
    public CraftingPromise requestCraftPromise(ItemKey item) {
        List<ItemKeyStack> stack = getCraftedItems();
        if (stack == null)
            return null;

        boolean found = false;
        ItemKeyStack craftingStack = null;
        for (ItemKeyStack craftable : stack) {
            craftingStack = craftable;
            if (craftingStack.key().equals(item)) {
                found = true;
                break;
            }
        }
        if (!found)
            return null;

        IWorldRequester[] requesters = new IWorldRequester[9];
        for (int i = 0; i < 9; i++)
            requesters[i] = this;

        CraftingPromise promise = new CraftingPromise(craftingStack, this, priority);
        for (int i = 0; i < 9; i++) {
            ItemKeyStack keystack = ItemKeyStack.get(matrix.getStackInSlot(i));
            if (keystack == null || keystack.stackSize <= 0)
                continue;
            promise.addIngredient(keystack, requesters[i]);
        }

        return promise;
    }

    @Override
    public void registerExcess(DeliveryPromise promise) {
        ItemKeyStack keystack = ItemKeyStack.get(promise.thePackage, promise.size);
        excess.add(new Pair2<ItemKeyStack, IWorldRequester>(keystack, null));
    }

    @Override
    public List<ItemKeyStack> getCraftedItems() {
        List<ItemKeyStack> list = new ArrayList<ItemKeyStack>(1);
        ItemStack stack = matrix.getStackInSlot(9);
        if (stack != null)
            list.add(ItemKeyStack.get(stack));
        return list;
    }

    @Override
    public int getPriority() {
        return priority;
    }

    @Override
    public double getWorkLoad() {
        return (manager.getTotalDeliveryCount() + 63.0) / 64.0;
    }

    @Override
    public int itemsToProcess() {
        return manager.getTotalDeliveryCount();
    }
    
    @Override
    public String getType() {
        return "pr_rcrafting";
    }

}
