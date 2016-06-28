package mrtjp.projectred.expansion

import java.util.UUID

import com.mojang.authlib.GameProfile
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.world.{World, WorldServer}
import net.minecraftforge.common.util.FakePlayerFactory

import scala.ref.WeakReference


object FakePlayerUtils {
  def getFakePlayerPR(world: World): WeakReference[EntityPlayerMP] = {
    WeakReference(FakePlayerFactory.get(world.asInstanceOf[WorldServer],
      new GameProfile(UUID.randomUUID(), "[PR_FAKE]")))
  }
}
