package mrtjp.projectred.utils;

import java.awt.image.BufferedImage;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.WeakHashMap;

import javax.imageio.ImageIO;

import net.minecraft.client.renderer.texture.Texture;
import net.minecraft.client.renderer.texture.TextureManager;
import net.minecraft.client.renderer.texture.TextureStitched;
import net.minecraft.client.texturepacks.ITexturePack;
import net.minecraft.client.texturepacks.TexturePackImplementation;
import net.minecraftforge.client.event.TextureStitchEvent;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.ForgeSubscribe;

import org.lwjgl.opengl.GL11;

import cpw.mods.fml.common.ObfuscationReflectionHelper;

public class TextureSlice extends TextureStitched {

	// name format is filepath>subID

	public TextureSlice(String name) {
		super(name);
		if (!name.contains(">")) {
			throw new AssertionError("Name must contain >");
		}
	}

	public static class TextureStitchListener {
		static TextureStitchListener instance = new TextureStitchListener();

		private TextureStitchListener() {
		}

		@ForgeSubscribe
		public void onTextureStitch(TextureStitchEvent.Post evt) {
			tpCache.clear();
		}
	}

	static {
		MinecraftForge.EVENT_BUS.register(TextureStitchListener.instance);
	}

	// pack -> (filename -> cache)
	private static WeakHashMap<ITexturePack, Map<String, TPCache>> tpCache = new WeakHashMap<ITexturePack, Map<String, TPCache>>();

	private static class TPCache {
		private static int parsePos(String s, BufferedImage im, int spriteSize) {
			int width = im.getWidth() / spriteSize;
			String[] p = s.split(",");
			if (p.length == 1)
				return Integer.parseInt(p[0]);
			else if (p.length == 2)
				return Integer.parseInt(p[0]) + width * Integer.parseInt(p[1]);
			else
				throw new NumberFormatException("Not a number or number,number: " + s);
		}

		TPCache(ITexturePack texturePack, String txtPath, String texPath) throws IOException {
			BufferedImage image;
			try {
				image = ImageIO.read(texturePack.getResourceAsStream("/" + texPath));
			} catch (FileNotFoundException e) {
				return;
			}

			try {
				Scanner s = new Scanner(texturePack.getResourceAsStream("/" + txtPath));
				try {
					spriteSize = s.nextInt();
					while (s.hasNextLine()) {
						String[] p = s.nextLine().split(" ");
						if (p.length == 2) {
							indices.put(p[0], parsePos(p[1], image, spriteSize));
						} else if (p.length == 3 && p[0].startsWith("*")) {
							int rep = Integer.parseInt(p[0].substring(1));
							String prefix = p[1];
							int base = parsePos(p[2], image, spriteSize);

							for (int k = 0; k < rep; k++)
								indices.put(prefix + k, base + k);
						}
					}
				} finally {
					s.close();
				}
			} catch (FileNotFoundException e) {
				return;
			}

			this.image = image;
		}

		Map<String, Integer> indices = new HashMap<String, Integer>();
		BufferedImage image;
		int spriteSize;
	}

	private boolean loadFrom(ITexturePack texturePack, String txtPath, String texPath, String subName, TextureManager manager, List<Texture> textures) throws IOException {
		TPCache texcache;
		synchronized (tpCache) {
			Map<String, TPCache> packCache = tpCache.get(tpCache);
			if (packCache == null)
				tpCache.put(texturePack, packCache = new HashMap<String, TPCache>());
			texcache = packCache.get(texPath);
			if (texcache == null) {
				packCache.put(texPath, texcache = new TPCache(texturePack, txtPath, texPath));
			}
		}

		if (texcache.image != null && texcache.indices.containsKey(subName)) {
			int index = texcache.indices.get(subName);
			int spriteSize = texcache.spriteSize;
			int widthSprites = (texcache.image.getWidth() / spriteSize);
			int x = (index % widthSprites) * spriteSize;
			int y = (index / widthSprites) * spriteSize;

			textures.add(manager.makeTexture(this.getIconName(), 2, spriteSize, spriteSize, GL11.GL_REPEAT, GL11.GL_RGBA, GL11.GL_NEAREST, GL11.GL_NEAREST, false, texcache.image.getSubimage(x, y, spriteSize, spriteSize)));
			return true;
		}

		return false;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public boolean loadTexture(TextureManager manager, ITexturePack texturepack, String name, String fileName, BufferedImage image, ArrayList textures) {

		// texture packs can have individual textures instead of sheets if they
		// want
		if (image != null)
			return false;

		String ext = fileName.substring(fileName.lastIndexOf('.') + 1);
		String baseName = fileName.substring(0, fileName.lastIndexOf('>'));

		String subName = fileName.substring(0, fileName.length() - ext.length() - 1).substring(baseName.length() + 1);

		// Note: x.sub.txt as opposed to x.txt (which is used for animation
		// data)
		String txtName = baseName + ".sub.txt";
		String texName = baseName + ".png";

		while (true) {
			try {
				if (loadFrom(texturepack, txtName, texName, subName, manager, textures))
					return true;
			} catch (IOException e) {
				e.printStackTrace();
			}

			if (texturepack instanceof TexturePackImplementation)
				texturepack = ObfuscationReflectionHelper.getPrivateValue(TexturePackImplementation.class, (TexturePackImplementation) texturepack, "field_98141_g", "fallbackTexturePack");
			else
				break;
		}

		return false;
	}

}
