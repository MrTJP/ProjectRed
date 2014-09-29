package mrtjp.projectred.expansion

import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.libmc.MultiTileRender
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.world.IBlockAccess

object RenderFurnace extends MultiTileRender(ProjectRedExpansion.machine1)
{
    override def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int){}

    override def renderInvBlock(r:RenderBlocks, meta:Int){}

    override def registerIcons(reg:IIconRegister){}

    override def getIcon(side:Int, meta:Int) = null
}