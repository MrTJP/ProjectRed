package mrtjp.projectred.expansion;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.SortedSet;
import java.util.TreeSet;

import mrtjp.projectred.core.utils.HashPair2;
import mrtjp.projectred.core.utils.ItemKey;
import mrtjp.projectred.core.utils.ItemKeyStack;
import mrtjp.projectred.core.utils.Pair2;
import mrtjp.projectred.expansion.RequestTree2.RequestFlags;

public class RequestTreeNode2 {

    public RequestTreeNode2(CraftingPromise parentCrafter, ItemKeyStack requestedPackage, IWorldRequester requester, RequestTreeNode2 parent, EnumSet<RequestFlags> type) {
        this.requestedPackage = requestedPackage;
        this.requester = requester;
        this.parent = parent;
        
        if (parent != null) {
            parent.subRequests.add(this);
            root = parent.root;
        } else
            root = (RequestTree2) this;
        
        if (parentCrafter != null) {
            if (!recurse_IsCrafterUsed(parentCrafter))
                usedCrafters.add(parentCrafter);
        }
        
        if (type.contains(RequestFlags.PULL) && getPromisesFromBroadcasters())
            return;
        
        if (type.contains(RequestFlags.CRAFT) && getPromisesFromExcess())
            return;
        
        if (type.contains(RequestFlags.CRAFT) && getPromisesFromCrafters())
            return;
    }
    
    private final ItemKeyStack requestedPackage;
    private final IWorldRequester requester;
    private final RequestTreeNode2 parent;
    protected final RequestTree2 root;
    
    private List<RequestTreeNode2> subRequests = new ArrayList<RequestTreeNode2>();
    private List<DeliveryPromise> promises = new ArrayList<DeliveryPromise>();
    private List<ExcessPromise> excessPromises = new ArrayList<ExcessPromise>();
    private SortedSet<CraftingPromise> usedCrafters = new TreeSet<CraftingPromise>();
    
    private CraftingPromise lastCrafterTried = null;

    private int promisedCount = 0;

    public int getPromisedCount() {
        return promisedCount;
    }
    
    public int getMissingCount() {
        return requestedPackage.stackSize - promisedCount;
    }
    
    public ItemKey getRequestedPackage() {
        return requestedPackage.key();
    }
    
    public boolean isDone() {
        return getMissingCount() <= 0;
    }
    
    public void addPromise(DeliveryPromise promise) {
        if (!promise.thePackage.equals(getRequestedPackage())) {
            //TODO
            System.out.println("NOT RIGHT PACKAGE " + promise.thePackage.makeStack(0).getDisplayName() + " vs " + getRequestedPackage().makeStack(0).getDisplayName());

            return;
        }

        if (promise.size > getMissingCount()) {
            int more = promise.size - getMissingCount();
            promise.size = getMissingCount();

            ExcessPromise excess = new ExcessPromise();
            excess.thePackage = promise.thePackage;
            excess.size = more;
            excess.sender = promise.sender;
            excessPromises.add(excess);
        }
        
        if (promise.size <= 0)
            return;
        
        promises.add(promise);
        promisedCount += promise.size;
        root.promiseAdded(promise);
        String s = promise instanceof ExcessPromise? "excess" : "";
        System.out.println("Added " + s + " promise " + promise.thePackage.makeStack(0).getDisplayName());
    }

    private boolean getPromisesFromCrafters() {
        //TODO
        System.out.println("starting crafters promise seeking");

        List<Router> allRouters = requester.getRouter().getRoutersByCost();
        List<CraftingPromise> allCrafters = new ArrayList<CraftingPromise>(allRouters.size());
        
        for (Router r : allRouters) {
            if (r.getParent() instanceof IWorldCrafter) {
                IWorldCrafter cr = (IWorldCrafter)r.getParent();
                CraftingPromise cp = cr.requestCraftPromise(getRequestedPackage());
                if (cp != null)
                    allCrafters.add(cp);
            }
        }

        //TODO
        System.out.println(allCrafters.size() + " Crafters found producing " +getRequestedPackage().makeStack(0).getDisplayName());
        for (CraftingPromise c : allCrafters)
            System.out.println("-- " + c.getResultItem().makeStack(0).getDisplayName());

        Iterator<CraftingPromise> allCraftersIt = allCrafters.iterator();
        
        PriorityQueue<CraftingTreeInteraction> craftersSamePriority = new PriorityQueue<CraftingTreeInteraction>(5);
        ArrayList<CraftingTreeInteraction> craftersToBalance = new ArrayList<CraftingTreeInteraction>();

        boolean recursionFinished = false;
        
        CraftingPromise lastCrafter = null;
        int currentPriority = 0;
        
        //TODO
        System.out.println("Starting crafting searching");

        while (!recursionFinished) {
            //TODO
            System.out.println("SEARCH RECURSE..");
            if (allCraftersIt.hasNext()) {
                if (lastCrafter == null)
                    lastCrafter = allCraftersIt.next();
            } else if (lastCrafter == null)
                recursionFinished = true;
            
            //TODO
            System.out.println("lastCrafter set to " + (lastCrafter==null?"null":lastCrafter.result.makeStack().getDisplayName()));
            
            int itemsNeeded = getMissingCount();
            //TODO
            System.out.println(itemsNeeded + " items still needed. ");

            if (lastCrafter != null && (craftersSamePriority.isEmpty() || currentPriority == lastCrafter.getPriority())) {
                System.out.println("Pulling crafter.");
                currentPriority = lastCrafter.getPriority();
                CraftingPromise crafter = lastCrafter;
                lastCrafter = null;
                if (recurse_IsCrafterUsed(crafter)) {
                    //TODO
                    System.out.println("Crafter " +crafter.result.makeStack().getDisplayName()+ " already used. contunuing");
                    continue;
                } else 
                    System.out.println("Crafter " +crafter.result.makeStack().getDisplayName()+ " NOT already used.");
                
                //TODO
                System.out.println("CTI added to samePriority list");
                CraftingTreeInteraction cti = new CraftingTreeInteraction(crafter, itemsNeeded, this);
                craftersSamePriority.add(cti);
                continue;
            }
            
            if (craftersToBalance.isEmpty() && (craftersSamePriority == null || craftersSamePriority.isEmpty())) {
                //TODO
                System.out.println("No crafters to balance or same crafters.");
                continue;
            }

            if (craftersSamePriority.size() == 1) {
                //TODO
                System.out.println("Only one crafter. ");
                craftersToBalance.add(craftersSamePriority.poll());
                craftersToBalance.get(0).addToWorkRequest(itemsNeeded);
            } else {
                //TODO
                System.out.println("Multiple crafters.");
                for (CraftingTreeInteraction c : craftersSamePriority)
                    System.out.println("-- " + c.crafter.getResultItem().makeStack(0).getDisplayName());

                if (!craftersSamePriority.isEmpty())
                    craftersToBalance.add(craftersSamePriority.poll());
                
                //TODO
                System.out.println("Starting crafter load balance.");
                while (!craftersToBalance.isEmpty() && itemsNeeded > 0) {
                    
                    while (!craftersSamePriority.isEmpty() && craftersSamePriority.peek().toDo() <= craftersToBalance.get(0).toDo())
                        craftersToBalance.add(craftersSamePriority.poll());                    
                    
                    int cap;
                    if (!craftersSamePriority.isEmpty())
                        cap = craftersSamePriority.peek().toDo();
                    else
                        cap = Integer.MAX_VALUE;
                    
                    int floor = craftersToBalance.get(0).toDo();
                    cap = Math.min(cap, floor + (itemsNeeded + craftersToBalance.size() - 1) / craftersToBalance.size());

                    Iterator<CraftingTreeInteraction> iter = craftersToBalance.iterator();
                    while (iter.hasNext()) {
                        CraftingTreeInteraction crafter = iter.next();
                        int request = Math.min(itemsNeeded, cap - floor);
                        if (request > 0) {
                            int craftingDone = crafter.addToWorkRequest(request);
                            itemsNeeded -= craftingDone;
                        }
                    }
                }
                //TODO
                System.out.println("Done balancing crafters via load. ");
            }
            //TODO
            System.out.println("Crafter balancing done. Now actually utilizing");
            Iterator<CraftingTreeInteraction> iter = craftersToBalance.iterator();
            while (iter.hasNext()) {
                CraftingTreeInteraction c = iter.next();
                if (c.setsRequested > 0 && !c.addWorkPromisesToTree())
                    iter.remove();
            }
            itemsNeeded = getMissingCount();

            if (itemsNeeded <= 0)
                break;

            if (!craftersToBalance.isEmpty())
                recursionFinished = false;
            
            //TODO
            System.out.println("loop end. " + (recursionFinished?"ALL DONE":"RECURSING"));
        }
        return isDone();
    }

    private boolean getPromisesFromExcess() {
        LinkedList<ExcessPromise> availableExcess = root.getAllExcessFor(getRequestedPackage());
        //TODO
        System.out.println("CHECKED EXCESS. " + availableExcess.size() + " excess found. ");
        for (ExcessPromise excess : availableExcess) {
            if (isDone())
                break;
            
            if (excess.size <= 0)
                continue;
            
            excess.size = Math.min(excess.size, getMissingCount());
            addPromise(excess);
        }
        
        return isDone();
    }
    
    private boolean getPromisesFromBroadcasters() {
        for (Router r : requester.getRouter().getRoutersByCost()) {
            if (isDone())
                break;
            
            IWorldRouter member = r.getParent();

            if (member.needsWork() || !(member instanceof IWorldBroadcaster))
                continue;
            IWorldBroadcaster member2 = (IWorldBroadcaster) member;
            member2.requestPromises(this, root.getExistingPromisesFor(new HashPair2<IWorldBroadcaster, ItemKey>(member2, getRequestedPackage())));
        }
        return isDone();
    }
    
    private int getPotentialSubPromises(int numberOfSets, CraftingPromise crafter) {
        boolean failed = false;
        int potentialSets = numberOfSets;
        
        List<Pair2<ItemKeyStack, IWorldRequester>> ingredients = crafter.getScaledIngredients(numberOfSets);
        ArrayList<RequestTreeNode2> children = new ArrayList<RequestTreeNode2>(ingredients.size());

        for (Pair2<ItemKeyStack, IWorldRequester> item : ingredients) {
            RequestTreeNode2 req = new RequestTreeNode2(crafter, item.getValue1(), item.getValue2(), this, RequestFlags.def);
            children.add(req);
            if (!req.isDone())
                failed = true;
        }
        
        if (failed) {
            for (RequestTreeNode2 sub : children)
                sub.destroy();
            
            lastCrafterTried = crafter;
            
            for (int i = 0; i < ingredients.size(); i++)
                potentialSets = Math.min(potentialSets, children.get(i).getPromisedCount() / (ingredients.get(i).getValue1().stackSize / numberOfSets));
            
            return getCalculatedSubPromises(potentialSets, crafter);
        }
        
        return potentialSets;
    }
    
    private int getCalculatedSubPromises(int numberOfSets, CraftingPromise crafter) {
        ArrayList<RequestTreeNode2> children = new ArrayList<RequestTreeNode2>();
        if (numberOfSets > 0) {
            List<Pair2<ItemKeyStack, IWorldRequester>> ingredients = crafter.getScaledIngredients(numberOfSets);
            boolean failed = false;
            
            for (Pair2<ItemKeyStack, IWorldRequester> item : ingredients) {
                RequestTreeNode2 req = new RequestTreeNode2(crafter, item.getValue1(), item.getValue2(), this, RequestFlags.def);
                children.add(req);
                if (!req.isDone())
                    failed = true;
            }
            
            if (failed) {
                for (RequestTreeNode2 sub : children)
                    sub.destroy();
                return 0;
            }
        }
        return numberOfSets;
    }
    
    private void destroy() {
        parent.remove(this);
    }

    private void remove(RequestTreeNode2 subNode) {
        subRequests.remove(subNode);
        subNode.recurse_RemoveSubPromisses();
    }
    
    protected void recurse_RemoveSubPromisses() {
        for(DeliveryPromise promise:promises)
            root.promiseRemoved(promise);
            
        for(RequestTreeNode2 subNode:subRequests)
            subNode.recurse_RemoveSubPromisses();
    }
    
    protected boolean recurse_IsCrafterUsed(CraftingPromise parentCrafter) {
        if (!usedCrafters.isEmpty() && usedCrafters.contains(parentCrafter))
            return true;
        if (parent == null)
            return false;
        return parent.recurse_IsCrafterUsed(parentCrafter);
    }

    protected void recurse_RequestDelivery() {
        for (RequestTreeNode2 subReq : subRequests)
            subReq.recurse_RequestDelivery();
        
        for (DeliveryPromise p : promises)
            p.sender.deliverPromises(p, requester);
        
        for (ExcessPromise p : excessPromises)
            if (p.sender instanceof IWorldCrafter)
                ((IWorldCrafter)p.sender).registerExcess(p);
            
    }
    
    protected void recurse_GatherExcess(ItemKey item, HashMap<IWorldBroadcaster, List<ExcessPromise>> excessMap) {
        for (ExcessPromise extra : excessPromises) {
            if (extra.thePackage == item) {
                List<ExcessPromise> extras = excessMap.get(extra.sender);
                if (extras == null) {
                    extras = new LinkedList<ExcessPromise>();
                    excessMap.put(extra.sender, extras);
                }
                extras.add(extra.copy());
            }
        }

        for (RequestTreeNode2 subNode : subRequests)
            subNode.recurse_GatherExcess(item, excessMap);
    }
    
    protected void recurse_RemoveUnusableExcess(ItemKey item, HashMap<IWorldBroadcaster, List<ExcessPromise>> excessMap) {
        for (DeliveryPromise promise : promises) {
            if (promise.thePackage != item)
                continue;

            if (!(promise instanceof ExcessPromise))
                continue;

            ExcessPromise epromise = (ExcessPromise) promise;

            if (epromise.used)
                continue;

            int usedcount = epromise.size;

            List<ExcessPromise> extras = excessMap.get(epromise.sender);
            if (extras == null)
                continue;
            
            Iterator<ExcessPromise> it = extras.iterator();
            while (it.hasNext()) {
                ExcessPromise extra = it.next();
                if (extra.size >= usedcount) {
                    extra.size -= usedcount;
                    usedcount = 0;
                    break;
                } else {
                    usedcount -= extra.size;
                    it.remove();
                }
            }
        }

        for (RequestTreeNode2 subNode : subRequests)
            subNode.recurse_RemoveUnusableExcess(item, excessMap);
    }

    public static class DeliveryPromise {
        public ItemKey thePackage;
        public int size;
        public IWorldBroadcaster sender;
        
        public DeliveryPromise setPackage(ItemKey thePackage) {
        	this.thePackage = thePackage;
        	return this;
        }
        public DeliveryPromise setSize(int size) {
        	this.size = size;
        	return this;
        }
        public DeliveryPromise setSender(IWorldBroadcaster sender) {
        	this.sender = sender;
        	return this;
        }
        
        public DeliveryPromise copy() {
            DeliveryPromise p = new DeliveryPromise();
            p.setPackage(thePackage).setSize(size).setSender(sender);
            return p;
        }
    }
    public static class ExcessPromise extends DeliveryPromise {
        boolean used;
        
        public ExcessPromise copy() {
            ExcessPromise p = new ExcessPromise();
            p.thePackage = thePackage;
            p.size = size;
            p.sender = sender;
            p.used = used;
            return p;
        }
    }
    public static class CraftingPromise implements Comparable<CraftingPromise> {
        private IWorldCrafter crafter;
        
        private ItemKeyStack result;
        private ArrayList<Pair2<ItemKeyStack, IWorldRequester>> ingredients = new ArrayList<Pair2<ItemKeyStack, IWorldRequester>>(9);
        
        private final int priority;
        
        public CraftingPromise(ItemKeyStack result, IWorldCrafter crafter, int priority) {
            this.result = result;
            this.crafter = crafter;
            this.priority = priority;
        }
        
        public IWorldCrafter getCrafter() {
            return crafter;
        }
        
        public int getPriority() {
            return priority;
        }
        
        public CraftingPromise addIngredient(ItemKeyStack ingredient, IWorldRequester crafter) {
            for (Pair2<ItemKeyStack, IWorldRequester> ing : ingredients)
                if (ing.getValue1().key().equals(ingredient.key()) && ing.getValue2() == crafter) {
                    ing.getValue1().stackSize += ingredient.stackSize;
                    return this;
                }
            ingredients.add(new Pair2<ItemKeyStack, IWorldRequester>(ingredient, crafter));
            return this;
        }
        
        public DeliveryPromise getScaledPromise(int numberOfSets) {
            DeliveryPromise p = new DeliveryPromise();
            p.thePackage = result.key().copy();
            p.size = result.stackSize * numberOfSets;
            p.sender = crafter;
            return p;
        }
        
        public List<Pair2<ItemKeyStack, IWorldRequester>> getScaledIngredients(int numberOfSets) {
            List<Pair2<ItemKeyStack, IWorldRequester>> components = new ArrayList<Pair2<ItemKeyStack, IWorldRequester>>(ingredients.size());
            
            for (Pair2<ItemKeyStack, IWorldRequester> ing : ingredients) {
                Pair2<ItemKeyStack, IWorldRequester> newIng = new Pair2<ItemKeyStack, IWorldRequester>(ing.getValue1().copy(), ing.getValue2());
                newIng.getValue1().stackSize *= numberOfSets;
                components.add(newIng);
            }
            
            return components;
        }

        @Override
        public int compareTo(CraftingPromise p2) {
            int c = priority - p2.priority;
            if (c == 0)
                c = result.compareTo(p2.result);
            if (c == 0)
                c = crafter.getRouter().compareTo(p2.crafter.getRouter());
            return c;
        }
        
        public boolean canCraft(ItemKey item) {
            return item.equals(result);
        }
        
        public int getSizeForSet() {
            return result.stackSize;
        }
        
        public ItemKey getResultItem() {
            return result.key();
        }
    }
    
    private class CraftingTreeInteraction implements Comparable<CraftingTreeInteraction> {
        private int setsRequested;
        private final int setSize;
        private final int maxSetsAvailable;
        private final RequestTreeNode2 treeNode;

        public final CraftingPromise crafter;
        public final int originalToDo;

        private CraftingTreeInteraction(CraftingPromise crafter, int maxToCraft, RequestTreeNode2 interaction) {
            this.crafter = crafter;
            this.treeNode = interaction;
            this.originalToDo = crafter.getCrafter().getWorkLoad();
            this.setsRequested = 0;
            this.setSize = crafter.getSizeForSet();
            this.maxSetsAvailable = (treeNode.getMissingCount() + setSize - 1) / setSize;
        }
        
        public int toDo() {
            return originalToDo + setsRequested * setSize;
        }
        
        private int calculateMaxPotentialSets(int maxSets) {
            int needed = 0;
            if (maxSets > 0)
                needed = maxSets;
            else
                needed = (treeNode.getMissingCount() + setSize - 1) / setSize;
            
            if (needed <= 0)
                return 0;
            
            return getPotentialSubPromises(needed, crafter);
        }
        
        public int addToWorkRequest(int extraWork) {
            int stacksRequested = (treeNode.getMissingCount() + setSize - 1) / setSize;
            setsRequested += stacksRequested;
            return stacksRequested*setSize;
        }

        public boolean addWorkPromisesToTree() {
            int setsToCraft = Math.min(setsRequested, maxSetsAvailable);
            int setsAbleToCraft = calculateMaxPotentialSets(setsToCraft);
            System.out.println("We are able to craft " + setsAbleToCraft + " sets of items");
            if (setsAbleToCraft > 0) {
                DeliveryPromise delivery = crafter.getScaledPromise(setsAbleToCraft);
                if (delivery.size != setsAbleToCraft*setSize)
                    return false;
                treeNode.addPromise(delivery);
            }

            boolean isDone = setsToCraft == setsAbleToCraft;
            setsRequested = 0;

            return isDone;
        }

        @Override
        public int compareTo(CraftingTreeInteraction o2) {
            return toDo() - o2.toDo();
        }
    }
}
