package mrtjp.projectred.core.libmc.fx;

import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;
import net.minecraft.client.renderer.texture.IIconRegister;
import net.minecraft.util.IIcon;
import net.minecraftforge.client.event.TextureStitchEvent;

import java.util.HashMap;

@SideOnly(Side.CLIENT)
public class ParticleIconRegistry
{
    public static final ParticleIconRegistry instance = new ParticleIconRegistry();
    private static final HashMap<String, IIcon> textures = new HashMap<String, IIcon>();

    public IIcon getIcon(String name)
    {
        IIcon icon = textures.get(name);
        if (icon != null)
            return icon;

        return textures.get("sparkle1");
    }

    @SubscribeEvent
    public void onIconRegistry(TextureStitchEvent.Pre event)
    {
        if (event.map.getTextureType() == 1)
        {
            reg = event.map;
            registerIcons();
            reg = null;
        }
    }

    private void registerIcons()
    {
        iconPut("sparkle1");
        iconPut("sparkle2");
        iconPut("ember");
        iconPut("box");
        iconPut("flutter1");
        iconPut("flutter2");
        iconPut("flutter3");
        iconPut("flutter4");
        iconPut("smoke");
    }

    private IIconRegister reg = null;
    private static final String textureDir = "projectred:particles/";

    private void iconPut(String s)
    {
        if (reg == null)
            return;

        IIcon icon = reg.registerIcon(textureDir + s);
        textures.put(s, icon);
    }
}
