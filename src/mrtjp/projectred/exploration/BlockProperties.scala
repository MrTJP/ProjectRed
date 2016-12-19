package mrtjp.projectred.exploration

import codechicken.lib.block.property.PropertyString
import net.minecraft.block.properties.PropertyBool
import net.minecraft.util.math.AxisAlignedBB

/**
  * Created by covers1624 on 16/12/2016.
  */
object BlockProperties {
    val ORE_TYPES = new PropertyString("type", OreDefs.createStringList)
    val STONE_TYPES = new PropertyString("type", DecorativeStoneDefs.createStringList)

    val WALL_AABB_BY_INDEX: Array[AxisAlignedBB] = Array[AxisAlignedBB](new AxisAlignedBB(0.25D, 0.0D, 0.25D, 0.75D, 1.0D, 0.75D), new AxisAlignedBB(0.25D, 0.0D, 0.25D, 0.75D, 1.0D, 1.0D), new AxisAlignedBB(0.0D, 0.0D, 0.25D, 0.75D, 1.0D, 0.75D), new AxisAlignedBB(0.0D, 0.0D, 0.25D, 0.75D, 1.0D, 1.0D), new AxisAlignedBB(0.25D, 0.0D, 0.0D, 0.75D, 1.0D, 0.75D), new AxisAlignedBB(0.3125D, 0.0D, 0.0D, 0.6875D, 0.875D, 1.0D), new AxisAlignedBB(0.0D, 0.0D, 0.0D, 0.75D, 1.0D, 0.75D), new AxisAlignedBB(0.0D, 0.0D, 0.0D, 0.75D, 1.0D, 1.0D), new AxisAlignedBB(0.25D, 0.0D, 0.25D, 1.0D, 1.0D, 0.75D), new AxisAlignedBB(0.25D, 0.0D, 0.25D, 1.0D, 1.0D, 1.0D), new AxisAlignedBB(0.0D, 0.0D, 0.3125D, 1.0D, 0.875D, 0.6875D), new AxisAlignedBB(0.0D, 0.0D, 0.25D, 1.0D, 1.0D, 1.0D), new AxisAlignedBB(0.25D, 0.0D, 0.0D, 1.0D, 1.0D, 0.75D), new AxisAlignedBB(0.25D, 0.0D, 0.0D, 1.0D, 1.0D, 1.0D), new AxisAlignedBB(0.0D, 0.0D, 0.0D, 1.0D, 1.0D, 0.75D), new AxisAlignedBB(0.0D, 0.0D, 0.0D, 1.0D, 1.0D, 1.0D))
    val WALL_CLIP_AABB_BY_INDEX: Array[AxisAlignedBB] = Array[AxisAlignedBB](WALL_AABB_BY_INDEX(0).setMaxY(1.5D), WALL_AABB_BY_INDEX(1).setMaxY(1.5D), WALL_AABB_BY_INDEX(2).setMaxY(1.5D), WALL_AABB_BY_INDEX(3).setMaxY(1.5D), WALL_AABB_BY_INDEX(4).setMaxY(1.5D), WALL_AABB_BY_INDEX(5).setMaxY(1.5D), WALL_AABB_BY_INDEX(6).setMaxY(1.5D), WALL_AABB_BY_INDEX(7).setMaxY(1.5D), WALL_AABB_BY_INDEX(8).setMaxY(1.5D), WALL_AABB_BY_INDEX(9).setMaxY(1.5D), WALL_AABB_BY_INDEX(10).setMaxY(1.5D), WALL_AABB_BY_INDEX(11).setMaxY(1.5D), WALL_AABB_BY_INDEX(12).setMaxY(1.5D), WALL_AABB_BY_INDEX(13).setMaxY(1.5D), WALL_AABB_BY_INDEX(14).setMaxY(1.5D), WALL_AABB_BY_INDEX(15).setMaxY(1.5D))
}
