package mrtjp.projectred.core.fx;

import java.util.HashMap;

import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.util.Icon;
import net.minecraftforge.client.event.TextureStitchEvent;
import net.minecraftforge.event.ForgeSubscribe;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class ParticleIconRegistry
{
    public static final ParticleIconRegistry instance = new ParticleIconRegistry();
    private static final HashMap<String, Icon> textures = new HashMap<String, Icon>();

    public Icon getIcon(String name) {
        Icon icon = textures.get(name);
        if (icon != null)
            return icon;

        return textures.get("sparkle1");
    }

    @ForgeSubscribe
    public void onIconRegistry(TextureStitchEvent.Pre event) {
        if (event.map.textureType == 1) {
            reg = event.map;
            registerIcons();
            reg = null;
        }
    }

    private void registerIcons() {
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

    private IconRegister reg = null;
    private static final String textureDir = "projectred:particles/";

    private void iconPut(String s) {
        if (reg == null)
            return;

        Icon icon = reg.registerIcon(textureDir + s);
        textures.put(s, icon);
    }
}
