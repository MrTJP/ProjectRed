package mrtjp.projectred.transportation

import codechicken.lib.vec.BlockCoord
import mrtjp.projectred.core.utils.{ItemKeyStack, ItemKey}
import mrtjp.projectred.transportation.SendPriority.SendPriority
import net.minecraft.inventory.IInventory
import net.minecraft.item.ItemStack
import net.minecraft.world.World
import scala.collection.immutable.BitSet
import scala.collection.mutable

trait IWorldRouter
{
    def getRouter:Router

    def needsWork:Boolean

    def refreshState:Boolean

    def getContainer:RoutedJunctionPipePart

    /** Item Syncing **/
    def itemEnroute(r:RoutedPayload)
    def itemArrived(r:RoutedPayload)
    def getSyncResponse(item:ItemKey, rival:SyncResponse):SyncResponse

    /** Item Requesting **/
    // Handled via subclasses.

    /** Item Broadcasting **/
    // Handled via subclasses.

    /** Item Crafting **/
    // Handled via subclasses.
}

trait IWorldRequester extends IWorldRouter
{
    def trackedItemLost(s:ItemKeyStack)

    def trackedItemReceived(s:ItemKeyStack)

    def getActiveFreeSpace(item:ItemKey):Int
}

trait IWorldBroadcaster extends IWorldRouter
{
    def requestPromises(request:RequestBranchNode, existingPromises:Int)

    def deliverPromises(promise:DeliveryPromise, requester:IWorldRequester)

    def getBroadcastedItems(map:mutable.HashMap[ItemKey, Int])

    def getPriority:Int

    def getWorkLoad:Double
}

trait IWorldCrafter extends IWorldRequester with IWorldBroadcaster
{
    def requestCraftPromise(item:ItemKey):CraftingPromise

    def registerExcess(promise:DeliveryPromise)

    def getCraftedItems:List[ItemKeyStack]

    def itemsToProcess:Int
}

trait IRouteLayer
{
    def queueStackToSend(stack:ItemStack, dirOfExtraction:Int, path:SyncResponse)
    def queueStackToSend(stack:ItemStack, dirOfExtraction:Int, priority:SendPriority, destination:Int)
    def getLogisticPath(stack:ItemKey, exclusions:BitSet, excludeStart:Boolean):SyncResponse

    def getRouter:Router
    def getWorldRouter:IWorldRouter
    def getBroadcaster:IWorldBroadcaster
    def getRequester:IWorldRequester

    def getWorld:World
    def getCoords:BlockCoord
}

trait IInventoryProvider
{
    def getInventory:IInventory

    def getInterfacedSide:Int
}
