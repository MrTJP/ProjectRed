package mrtjp.projectred.expansion;


public interface ITubeInterface extends ITubeConnectable {

    /**
     * The routing weight for this pipe. Item routing will find the path with
     * LEAST cost.
     */
    public int getGCost();

    /**
     * True if this interface is connected to a pipe in the direction.
     */
    public boolean maskConnects(int absDir);

    /**
     * True if this can accept the item. This should do checking such as color
     * checking, filter checking, etc. This is for passing items through, not a
     * destination. (Something like a valve / tube, where items aren't actually
     * stored here, but can pass through here.)
     */
    public boolean canAcceptItem(TubeItem item, int onSide);

    /**
     * True if item can come and be stored here as a destination. When routing
     * items, this will be considered as a destination, and if its the cheapest
     * route, it will be taken.
     * 
     * @param item
     * @param onSide
     * @return
     */
    public boolean isDestinationForItem(TubeItem item, int onSide);
    
    /**
     * Add the item to the tube flow, can be from another pipe or a machine.
     * @param item
     * @param onSide
     * @return If the item was added.
     */
    public boolean addItem(TubeItem item, int onSide);
}
