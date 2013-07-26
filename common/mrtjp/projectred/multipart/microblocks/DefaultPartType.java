package mrtjp.projectred.multipart.microblocks;

import java.io.DataInput;

import mrtjp.projectred.utils.Dir;
import net.minecraft.block.Block;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.RenderGlobal;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.texture.TextureMap;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.Icon;

import org.lwjgl.opengl.GL11;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class DefaultPartType implements PartType<Part> {
	@Override
	public boolean canHarvest(EntityPlayer ply, Part part) {
		return modelBlock.canHarvestBlock(ply, modelMeta);
	}

	@Override
	public EnumPartClass getPartClass() {
		return clazz;
	}

	@Override
	public double getSize() {
		return size;
	}

	@Override
	public String getUnlocalizedName(ItemStack stack) {
		return unlocalizedName;
	}

	@Override
	public int getID() {
		return id;
	}

	@Override
	public ItemStack getDroppedStack(Part part, EntityPlayer ply) {
		return MicroblockLibrary.instance.getMicroblockSystem().partTypeIDToItemStack(id, 1);
	}

	@Override
	public float getPlayerRelativeHardness(Part part, EntityPlayer ply) {
		float hardness;
		try {
			hardness = modelBlock.getBlockHardness(null, 0, 0, 0);
		} catch (Throwable t) {
			return 0.1f;
		}

		if (hardness < 0)
			return 0;
		if (!canHarvest(ply, part))
			return 0.01F / hardness;
		return ply.getCurrentPlayerStrVsBlock(modelBlock, false, modelMeta) / hardness / 30F;
	}

	@Override
	public ItemStack getPickItem(Part part) {
		return getDroppedStack(part, null);
	}

	@Override
	public Part createPart(EnumPosition pos) {
		return new Part(this, pos);
	}

	@Override
	@SideOnly(Side.CLIENT)
	public Part createPart(EnumPosition pos, DataInput data) {
		return createPart(pos);
	}

	@Override
	public Part createPart(EnumPosition pos, NBTTagCompound c) {
		return createPart(pos);
	}

	private final EnumPartClass clazz;
	private final double size;
	private final String unlocalizedName;
	private final int id;

	public DefaultPartType(int id, EnumPartClass clazz, double size, String unlocalizedName, Block modelBlock, int modelMeta) {
		this.clazz = clazz;
		this.size = size;
		this.id = id;
		this.unlocalizedName = unlocalizedName;
		this.modelBlock = modelBlock;
		this.modelMeta = modelMeta;
	}

	public Block modelBlock;
	public int modelMeta;

	@SideOnly(Side.CLIENT)
	private float uMin, uMax, vMin, vMax;
	
	@SideOnly(Side.CLIENT)
	private Icon icon, overrideIcon;

	@SideOnly(Side.CLIENT)
	private void setTex(Icon i) {
		if (overrideIcon != null)
			i = overrideIcon;
		icon = i;
		uMin = i.getMinU();
		uMax = i.getMaxU();
		vMin = i.getMinV();
		vMax = i.getMaxV();
	}

	@SideOnly(Side.CLIENT)
	private void setTex(int side) {
		setTex(modelBlock.getIcon(side, modelMeta));
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void renderPreview(RenderGlobal context, EnumPosition pos, ItemStack stack) {
		GL11.glEnable(GL11.GL_BLEND);
		GL11.glColor4f(1.0f, 1.0f, 1.0f, 0.5f);

		overrideIcon = null;
		Minecraft.getMinecraft().renderEngine.func_110577_a(TextureMap.field_110575_b);
		Tessellator t = Tessellator.instance;
		t.startDrawingQuads();
		renderQuads(t, pos);
		t.draw();

		GL11.glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
		GL11.glDisable(GL11.GL_BLEND);
	}

	private static final double HOLLOW_EDGE_SIZE = 0.25;

	@SideOnly(Side.CLIENT)
	private void vertXY(Tessellator t, double x, double y, double z) {
		t.addVertexWithUV(x, y, z, uMin + (uMax - uMin) * x, vMax + (vMin - vMax) * y);
	}

	@SideOnly(Side.CLIENT)
	private void vertXZ(Tessellator t, double x, double y, double z) {
		t.addVertexWithUV(x, y, z, uMin + (uMax - uMin) * x, vMin + (vMax - vMin) * z);
	}

	@SideOnly(Side.CLIENT)
	private void vertYZ(Tessellator t, double x, double y, double z) {
		t.addVertexWithUV(x, y, z, uMin + (uMax - uMin) * z, vMax + (vMin - vMax) * y);
	}

	@SideOnly(Side.CLIENT)
	private void renderHollowPanelYZ(Tessellator t, AxisAlignedBB aabb) {
		setTex(modelBlock.getIcon(Dir.NX, modelMeta));
		t.setNormal(-1, 0, 0);

		vertYZ(t, aabb.minX, 0 + HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.minX, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.minX, 1, 0);
		vertYZ(t, aabb.minX, 0, 0);

		vertYZ(t, aabb.minX, 0, 1);
		vertYZ(t, aabb.minX, 1, 1);
		vertYZ(t, aabb.minX, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.minX, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);

		vertYZ(t, aabb.minX, 0, 1);
		vertYZ(t, aabb.minX, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.minX, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.minX, 0, 0);

		vertYZ(t, aabb.minX, 1, 0);
		vertYZ(t, aabb.minX, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.minX, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.minX, 1, 1);

		setTex(modelBlock.getIcon(Dir.PX, modelMeta));
		t.setNormal(1, 0, 0);

		vertYZ(t, aabb.maxX, 0, 0);
		vertYZ(t, aabb.maxX, 1, 0);
		vertYZ(t, aabb.maxX, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.maxX, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);

		vertYZ(t, aabb.maxX, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.maxX, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.maxX, 1, 1);
		vertYZ(t, aabb.maxX, 0, 1);

		vertYZ(t, aabb.maxX, 0, 0);
		vertYZ(t, aabb.maxX, HOLLOW_EDGE_SIZE, 0 + HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.maxX, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.maxX, 0, 1);

		vertYZ(t, aabb.maxX, 1, 1);
		vertYZ(t, aabb.maxX, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.maxX, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertYZ(t, aabb.maxX, 1, 0);

		setTex(modelBlock.getIcon(Dir.NY, modelMeta));
		t.setNormal(0, -1, 0);
		vertXZ(t, aabb.minX, 0, 0);
		vertXZ(t, aabb.maxX, 0, 0);
		vertXZ(t, aabb.maxX, 0, 1);
		vertXZ(t, aabb.minX, 0, 1);

		vertXZ(t, aabb.minX, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertXZ(t, aabb.maxX, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertXZ(t, aabb.maxX, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertXZ(t, aabb.minX, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);

		setTex(modelBlock.getIcon(Dir.PY, modelMeta));
		t.setNormal(0, 1, 0);
		vertXZ(t, aabb.minX, 1, 1);
		vertXZ(t, aabb.maxX, 1, 1);
		vertXZ(t, aabb.maxX, 1, 0);
		vertXZ(t, aabb.minX, 1, 0);

		vertXZ(t, aabb.minX, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertXZ(t, aabb.maxX, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertXZ(t, aabb.maxX, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertXZ(t, aabb.minX, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);

		setTex(modelBlock.getIcon(Dir.NZ, modelMeta));
		t.setNormal(0, 0, -1);
		vertXY(t, aabb.minX, 0, 0);
		vertXY(t, aabb.minX, 1, 0);
		vertXY(t, aabb.maxX, 1, 0);
		vertXY(t, aabb.maxX, 0, 0);

		vertXY(t, aabb.minX, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertXY(t, aabb.minX, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertXY(t, aabb.maxX, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);
		vertXY(t, aabb.maxX, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE);

		setTex(modelBlock.getIcon(Dir.PZ, modelMeta));
		t.setNormal(0, 0, 1);
		vertXY(t, aabb.minX, 1, 1);
		vertXY(t, aabb.minX, 0, 1);
		vertXY(t, aabb.maxX, 0, 1);
		vertXY(t, aabb.maxX, 1, 1);

		vertXY(t, aabb.minX, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertXY(t, aabb.minX, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertXY(t, aabb.maxX, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
		vertXY(t, aabb.maxX, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE);
	}

	@SideOnly(Side.CLIENT)
	private void renderHollowPanelXZ(Tessellator t, AxisAlignedBB aabb) {
		setTex(Dir.NY);
		t.setNormal(0, -1, 0);
		vertXZ(t, HOLLOW_EDGE_SIZE, aabb.minY, HOLLOW_EDGE_SIZE);
		vertXZ(t, HOLLOW_EDGE_SIZE, aabb.minY, 1 - HOLLOW_EDGE_SIZE);
		vertXZ(t, 0, aabb.minY, 1);
		vertXZ(t, 0, aabb.minY, 0);

		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.minY, 1 - HOLLOW_EDGE_SIZE);
		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.minY, HOLLOW_EDGE_SIZE);
		vertXZ(t, 1, aabb.minY, 0);
		vertXZ(t, 1, aabb.minY, 1);

		vertXZ(t, 0, aabb.minY, 0);
		vertXZ(t, 1, aabb.minY, 0);
		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.minY, HOLLOW_EDGE_SIZE);
		vertXZ(t, HOLLOW_EDGE_SIZE, aabb.minY, HOLLOW_EDGE_SIZE);

		vertXZ(t, HOLLOW_EDGE_SIZE, aabb.minY, 1 - HOLLOW_EDGE_SIZE);
		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.minY, 1 - HOLLOW_EDGE_SIZE);
		vertXZ(t, 1, aabb.minY, 1);
		vertXZ(t, 0, aabb.minY, 1);

		setTex(Dir.PY);
		t.setNormal(0, 1, 0);
		vertXZ(t, 0, aabb.maxY, 0);
		vertXZ(t, 0, aabb.maxY, 1);
		vertXZ(t, HOLLOW_EDGE_SIZE, aabb.maxY, 1 - HOLLOW_EDGE_SIZE);
		vertXZ(t, HOLLOW_EDGE_SIZE, aabb.maxY, HOLLOW_EDGE_SIZE);

		vertXZ(t, 1, aabb.maxY, 1);
		vertXZ(t, 1, aabb.maxY, 0);
		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.maxY, HOLLOW_EDGE_SIZE);
		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.maxY, 1 - HOLLOW_EDGE_SIZE);

		vertXZ(t, HOLLOW_EDGE_SIZE, aabb.maxY, HOLLOW_EDGE_SIZE);
		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.maxY, HOLLOW_EDGE_SIZE);
		vertXZ(t, 1, aabb.maxY, 0);
		vertXZ(t, 0, aabb.maxY, 0);

		vertXZ(t, 0, aabb.maxY, 1);
		vertXZ(t, 1, aabb.maxY, 1);
		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.maxY, 1 - HOLLOW_EDGE_SIZE);
		vertXZ(t, HOLLOW_EDGE_SIZE, aabb.maxY, 1 - HOLLOW_EDGE_SIZE);

		setTex(Dir.NX);
		t.setNormal(-1, 0, 0);
		vertYZ(t, 0, aabb.minY, 0);
		vertYZ(t, 0, aabb.minY, 1);
		vertYZ(t, 0, aabb.maxY, 1);
		vertYZ(t, 0, aabb.maxY, 0);

		vertYZ(t, HOLLOW_EDGE_SIZE, aabb.minY, 1 - HOLLOW_EDGE_SIZE);
		vertYZ(t, HOLLOW_EDGE_SIZE, aabb.minY, HOLLOW_EDGE_SIZE);
		vertYZ(t, HOLLOW_EDGE_SIZE, aabb.maxY, HOLLOW_EDGE_SIZE);
		vertYZ(t, HOLLOW_EDGE_SIZE, aabb.maxY, 1 - HOLLOW_EDGE_SIZE);

		setTex(Dir.PX);
		t.setNormal(1, 0, 0);
		vertYZ(t, 1, aabb.minY, 1);
		vertYZ(t, 1, aabb.minY, 0);
		vertYZ(t, 1, aabb.maxY, 0);
		vertYZ(t, 1, aabb.maxY, 1);

		vertYZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.maxY, 1 - HOLLOW_EDGE_SIZE);
		vertYZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.maxY, HOLLOW_EDGE_SIZE);
		vertYZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.minY, HOLLOW_EDGE_SIZE);
		vertYZ(t, 1 - HOLLOW_EDGE_SIZE, aabb.minY, 1 - HOLLOW_EDGE_SIZE);

		setTex(Dir.NZ);
		t.setNormal(0, 0, -1);
		vertXY(t, 0, aabb.minY, 0);
		vertXY(t, 0, aabb.maxY, 0);
		vertXY(t, 1, aabb.maxY, 0);
		vertXY(t, 1, aabb.minY, 0);

		vertXY(t, 1 - HOLLOW_EDGE_SIZE, aabb.minY, HOLLOW_EDGE_SIZE);
		vertXY(t, 1 - HOLLOW_EDGE_SIZE, aabb.maxY, HOLLOW_EDGE_SIZE);
		vertXY(t, HOLLOW_EDGE_SIZE, aabb.maxY, HOLLOW_EDGE_SIZE);
		vertXY(t, HOLLOW_EDGE_SIZE, aabb.minY, HOLLOW_EDGE_SIZE);

		setTex(Dir.PZ);
		t.setNormal(0, 0, 1);
		vertXY(t, 1, aabb.minY, 1);
		vertXY(t, 1, aabb.maxY, 1);
		vertXY(t, 0, aabb.maxY, 1);
		vertXY(t, 0, aabb.minY, 1);

		vertXY(t, HOLLOW_EDGE_SIZE, aabb.minY, 1 - HOLLOW_EDGE_SIZE);
		vertXY(t, HOLLOW_EDGE_SIZE, aabb.maxY, 1 - HOLLOW_EDGE_SIZE);
		vertXY(t, 1 - HOLLOW_EDGE_SIZE, aabb.maxY, 1 - HOLLOW_EDGE_SIZE);
		vertXY(t, 1 - HOLLOW_EDGE_SIZE, aabb.minY, 1 - HOLLOW_EDGE_SIZE);
	}

	@SideOnly(Side.CLIENT)
	private void renderHollowPanelXY(Tessellator t, AxisAlignedBB aabb) {
		setTex(Dir.NZ);
		t.setNormal(0, 0, -1);
		vertXY(t, 0, 0, aabb.minZ);
		vertXY(t, 0, 1, aabb.minZ);
		vertXY(t, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.minZ);
		vertXY(t, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.minZ);

		vertXY(t, 1, 1, aabb.minZ);
		vertXY(t, 1, 0, aabb.minZ);
		vertXY(t, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.minZ);
		vertXY(t, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.minZ);

		vertXY(t, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.minZ);
		vertXY(t, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.minZ);
		vertXY(t, 1, 0, aabb.minZ);
		vertXY(t, 0, 0, aabb.minZ);

		vertXY(t, 0, 1, aabb.minZ);
		vertXY(t, 1, 1, aabb.minZ);
		vertXY(t, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.minZ);
		vertXY(t, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.minZ);

		setTex(Dir.PZ);
		t.setNormal(0, 0, 1);
		vertXY(t, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertXY(t, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertXY(t, 0, 1, aabb.maxZ);
		vertXY(t, 0, 0, aabb.maxZ);

		vertXY(t, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertXY(t, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertXY(t, 1, 0, aabb.maxZ);
		vertXY(t, 1, 1, aabb.maxZ);

		vertXY(t, 0, 0, aabb.maxZ);
		vertXY(t, 1, 0, aabb.maxZ);
		vertXY(t, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertXY(t, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.maxZ);

		vertXY(t, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertXY(t, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertXY(t, 1, 1, aabb.maxZ);
		vertXY(t, 0, 1, aabb.maxZ);

		setTex(Dir.NX);
		t.setNormal(-1, 0, 0);
		vertYZ(t, 0, 1, aabb.minZ);
		vertYZ(t, 0, 0, aabb.minZ);
		vertYZ(t, 0, 0, aabb.maxZ);
		vertYZ(t, 0, 1, aabb.maxZ);

		vertYZ(t, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.minZ);
		vertYZ(t, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.minZ);
		vertYZ(t, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertYZ(t, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.maxZ);

		setTex(Dir.PX);
		t.setNormal(1, 0, 0);
		vertYZ(t, 1, 0, aabb.minZ);
		vertYZ(t, 1, 1, aabb.minZ);
		vertYZ(t, 1, 1, aabb.maxZ);
		vertYZ(t, 1, 0, aabb.maxZ);

		vertYZ(t, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertYZ(t, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertYZ(t, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.minZ);
		vertYZ(t, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.minZ);

		setTex(Dir.NY);
		t.setNormal(0, -1, 0);
		vertXZ(t, 1, 0, aabb.minZ);
		vertXZ(t, 1, 0, aabb.maxZ);
		vertXZ(t, 0, 0, aabb.maxZ);
		vertXZ(t, 0, 0, aabb.minZ);

		vertXZ(t, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.minZ);
		vertXZ(t, HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, HOLLOW_EDGE_SIZE, aabb.minZ);

		setTex(Dir.PY);
		t.setNormal(0, 1, 0);
		vertXZ(t, 0, 1, aabb.minZ);
		vertXZ(t, 0, 1, aabb.maxZ);
		vertXZ(t, 1, 1, aabb.maxZ);
		vertXZ(t, 1, 1, aabb.minZ);

		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.minZ);
		vertXZ(t, 1 - HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertXZ(t, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.maxZ);
		vertXZ(t, HOLLOW_EDGE_SIZE, 1 - HOLLOW_EDGE_SIZE, aabb.minZ);
	}

	@SideOnly(Side.CLIENT)
	private void renderQuads(Tessellator t, EnumPosition pos) {
		AxisAlignedBB aabb = Part.getBoundingBoxFromPool(pos, size);

		if (getPartClass() == EnumPartClass.HollowPanel) {
			if (pos.x != EnumAxisPosition.Span)
				renderHollowPanelYZ(t, aabb);
			else if (pos.y != EnumAxisPosition.Span)
				renderHollowPanelXZ(t, aabb);
			else if (pos.z != EnumAxisPosition.Span)
				renderHollowPanelXY(t, aabb);
			return;
		}

		renderAABB(t, aabb, null, 0, 0, 0, null, new boolean[6]);
	}

	// colour and brightness values for the corners of the face currently being
	// rendered
	@SideOnly(Side.CLIENT)
	private int colNN, colPN, colNP, colPP;
	@SideOnly(Side.CLIENT)
	private int briNN, briPN, briNP, briPP;

	@SideOnly(Side.CLIENT)
	private static double interp(double a, double b, double i) {
		return a + (b - a) * i;
	}

	@SideOnly(Side.CLIENT)
	private static int rgb_component(double a) {
		if (a < 0)
			return 0;
		if (a > 1)
			return 255;
		return (int) (255 * a);
	}

	@SideOnly(Side.CLIENT)
	private static int rgb(double r, double g, double b) {
		return (rgb_component(r) << 16) | (rgb_component(g) << 8) | rgb_component(b);
	}

	@SideOnly(Side.CLIENT)
	private static int rgb_r(int a) {
		return (a >> 16) & 255;
	}

	@SideOnly(Side.CLIENT)
	private static int rgb_g(int a) {
		return (a >> 8) & 255;
	}

	@SideOnly(Side.CLIENT)
	private static int rgb_b(int a) {
		return a & 255;
	}

	@SideOnly(Side.CLIENT)
	private static int interpRGB(int a, int b, double i) {
		return rgb(interp(rgb_r(a), rgb_r(b), i) / 255.0, interp(rgb_g(a), rgb_g(b), i) / 255.0, interp(rgb_b(a), rgb_b(b), i) / 255.0);
	}

	@SideOnly(Side.CLIENT)
	private static int scaleRGB(int a, double sc) {
		sc /= 255;
		return rgb(rgb_r(a) * sc, rgb_g(a) * sc, rgb_b(a) * sc);
	}

	@SideOnly(Side.CLIENT)
	private static int interpBrightness(int a, int b, double i) {
		int AH = a >> 16;
		int AL = a & 65535;
		int BH = b >> 16;
		int BL = b & 65535;
		int H = (int) (AH + (BH - AH) * i);
		int L = (int) (AL + (BL - AL) * i);
		return (H << 16) | L;
	}

	@SideOnly(Side.CLIENT)
	private void setColourAndBrightness(Tessellator t, double x, double y) {
		int col = interpRGB(interpRGB(colNN, colPN, x), interpRGB(colNP, colPP, x), y);
		int bri = interpBrightness(interpBrightness(briNN, briPN, x), interpBrightness(briNP, briPP, x), y);
		t.setColorOpaque_I(col);
		t.setBrightness(bri);
	}

	@SideOnly(Side.CLIENT)
	private void getBrightnessXFace(RenderBlocks rb, int bx, int by, int bz, double f_dx) {
		int dx = (f_dx >= 1 ? 1 : f_dx <= 0 ? -1 : 0);
		if (rb == null) {
			briNN = briNP = briPN = briPP = 0x00F000F0;
		} else {
			bx += dx;
			briNN = briNP = briPN = briPP = rb.blockAccess.getLightBrightnessForSkyBlocks(bx, by, bz, 0);
		}
	}

	@SideOnly(Side.CLIENT)
	private void getBrightnessYFace(RenderBlocks rb, int bx, int by, int bz, double f_dy) {
		int dy = (f_dy >= 1 ? 1 : f_dy <= 0 ? -1 : 0);
		if (rb == null) {
			briNN = briNP = briPN = briPP = 0x00F000F0;
		} else {
			by += dy;
			briNN = briNP = briPN = briPP = rb.blockAccess.getLightBrightnessForSkyBlocks(bx, by, bz, 0);
		}
	}

	@SideOnly(Side.CLIENT)
	private void getBrightnessZFace(RenderBlocks rb, int bx, int by, int bz, double f_dz) {
		int dz = (f_dz >= 1 ? 1 : f_dz <= 0 ? -1 : 0);
		if (rb == null) {
			briNN = briNP = briPN = briPP = 0x00F000F0;
		} else {
			bz += dz;
			briNN = briNP = briPN = briPP = rb.blockAccess.getLightBrightnessForSkyBlocks(bx, by, bz, 0);
		}
	}

	@SideOnly(Side.CLIENT)
	private void renderAABB(Tessellator t, AxisAlignedBB aabb, RenderBlocks rb, int bx, int by, int bz, Block par1Block, boolean[] dontRenderFaces) {

		// block-relative AABB
		AxisAlignedBB rbb = aabb.getOffsetBoundingBox(-bx, -by, -bz);

		// texture offsets from fractional coordinates
		double txmin = (((aabb.minX % 1) + 1) % 1);
		double txmax = (((aabb.maxX % 1) + 1) % 1);
		double tymin = (((aabb.minY % 1) + 1) % 1);
		double tymax = (((aabb.maxY % 1) + 1) % 1);
		double tzmin = (((aabb.minZ % 1) + 1) % 1);
		double tzmax = (((aabb.maxZ % 1) + 1) % 1);

		if (txmax == 0)
			txmax = 1;
		if (tymax == 0)
			tymax = 1;
		if (tzmax == 0)
			tzmax = 1;

		float colourR = 1, colourG = 1, colourB = 1;

		// minimum and maximum X/Y/Z values for dontRenderFaces to not apply
		// (anything below DRFMIN is treated as 0, anything above DRFMAX treated
		// as 1)
		final double DRFMIN = 0.005;
		final double DRFMAX = 0.995;

		if (tzmin > DRFMIN || !dontRenderFaces[Dir.NZ]) {
			setTex(Dir.NZ);
			t.setNormal(0, 0, -1);
			colNN = colPN = colNP = colPP = rgb(colourR * 0.8, colourG * 0.8, colourB * 0.8);
			getBrightnessZFace(rb, bx, by, bz, rbb.minZ);
			setColourAndBrightness(t, rbb.minX, rbb.minY);
			t.addVertexWithUV(aabb.minX, aabb.minY, aabb.minZ, uMax + txmin * (uMin - uMax), vMax + tymin * (vMin - vMax));
			setColourAndBrightness(t, rbb.minX, rbb.maxY);
			t.addVertexWithUV(aabb.minX, aabb.maxY, aabb.minZ, uMax + txmin * (uMin - uMax), vMax + tymax * (vMin - vMax));
			setColourAndBrightness(t, rbb.maxX, rbb.maxY);
			t.addVertexWithUV(aabb.maxX, aabb.maxY, aabb.minZ, uMax + txmax * (uMin - uMax), vMax + tymax * (vMin - vMax));
			setColourAndBrightness(t, rbb.maxX, rbb.minY);
			t.addVertexWithUV(aabb.maxX, aabb.minY, aabb.minZ, uMax + txmax * (uMin - uMax), vMax + tymin * (vMin - vMax));
		}

		if (tymin > DRFMIN || !dontRenderFaces[Dir.NY]) {
			setTex(Dir.NY);
			colNN = colPN = colNP = colPP = rgb(colourR * 0.5, colourG * 0.5, colourB * 0.5);
			t.setNormal(0, -1, 0);
			getBrightnessYFace(rb, bx, by, bz, rbb.minY);
			setColourAndBrightness(t, rbb.maxX, rbb.minZ);
			t.addVertexWithUV(aabb.maxX, aabb.minY, aabb.minZ, uMin + txmax * (uMax - uMin), vMin + tzmin * (vMax - vMin));
			setColourAndBrightness(t, rbb.maxX, rbb.maxZ);
			t.addVertexWithUV(aabb.maxX, aabb.minY, aabb.maxZ, uMin + txmax * (uMax - uMin), vMin + tzmax * (vMax - vMin));
			setColourAndBrightness(t, rbb.minX, rbb.maxZ);
			t.addVertexWithUV(aabb.minX, aabb.minY, aabb.maxZ, uMin + txmin * (uMax - uMin), vMin + tzmax * (vMax - vMin));
			setColourAndBrightness(t, rbb.minX, rbb.minZ);
			t.addVertexWithUV(aabb.minX, aabb.minY, aabb.minZ, uMin + txmin * (uMax - uMin), vMin + tzmin * (vMax - vMin));
		}

		if (txmin > DRFMIN || !dontRenderFaces[Dir.NX]) {
			setTex(Dir.NX);
			t.setNormal(-1, 0, 0);
			colNN = colPN = colNP = colPP = rgb(colourR * 0.6, colourG * 0.6, colourB * 0.6);
			getBrightnessXFace(rb, bx, by, bz, rbb.minX);
			setColourAndBrightness(t, rbb.minY, rbb.minZ);
			t.addVertexWithUV(aabb.minX, aabb.minY, aabb.minZ, uMin + tzmin * (uMax - uMin), vMax + tymin * (vMin - vMax));
			setColourAndBrightness(t, rbb.minY, rbb.maxZ);
			t.addVertexWithUV(aabb.minX, aabb.minY, aabb.maxZ, uMin + tzmax * (uMax - uMin), vMax + tymin * (vMin - vMax));
			setColourAndBrightness(t, rbb.maxY, rbb.maxZ);
			t.addVertexWithUV(aabb.minX, aabb.maxY, aabb.maxZ, uMin + tzmax * (uMax - uMin), vMax + tymax * (vMin - vMax));
			setColourAndBrightness(t, rbb.maxY, rbb.minZ);
			t.addVertexWithUV(aabb.minX, aabb.maxY, aabb.minZ, uMin + tzmin * (uMax - uMin), vMax + tymax * (vMin - vMax));
		}

		if (tzmax < DRFMAX || !dontRenderFaces[Dir.PZ]) {
			setTex(Dir.PZ);
			t.setNormal(0, 0, 1);
			colNN = colPN = colNP = colPP = rgb(colourR * 0.8, colourG * 0.8, colourB * 0.8);
			getBrightnessZFace(rb, bx, by, bz, rbb.maxZ);
			setColourAndBrightness(t, rbb.maxX, rbb.minY);
			t.addVertexWithUV(aabb.maxX, aabb.minY, aabb.maxZ, uMin + txmax * (uMax - uMin), vMax + tymin * (vMin - vMax));
			setColourAndBrightness(t, rbb.maxX, rbb.maxY);
			t.addVertexWithUV(aabb.maxX, aabb.maxY, aabb.maxZ, uMin + txmax * (uMax - uMin), vMax + tymax * (vMin - vMax));
			setColourAndBrightness(t, rbb.minX, rbb.maxY);
			t.addVertexWithUV(aabb.minX, aabb.maxY, aabb.maxZ, uMin + txmin * (uMax - uMin), vMax + tymax * (vMin - vMax));
			setColourAndBrightness(t, rbb.minX, rbb.minY);
			t.addVertexWithUV(aabb.minX, aabb.minY, aabb.maxZ, uMin + txmin * (uMax - uMin), vMax + tymin * (vMin - vMax));
		}

		if (tymax < DRFMAX || !dontRenderFaces[Dir.PY]) {
			setTex(Dir.PY);
			t.setNormal(0, 1, 0);
			colNN = colPN = colNP = colPP = rgb(colourR * 1.0, colourG * 1.0, colourB * 1.0);
			getBrightnessYFace(rb, bx, by, bz, rbb.maxY);
			setColourAndBrightness(t, rbb.minX, rbb.minZ);
			t.addVertexWithUV(aabb.minX, aabb.maxY, aabb.minZ, uMin + txmin * (uMax - uMin), vMin + tzmin * (vMax - vMin));
			setColourAndBrightness(t, rbb.minX, rbb.maxZ);
			t.addVertexWithUV(aabb.minX, aabb.maxY, aabb.maxZ, uMin + txmin * (uMax - uMin), vMin + tzmax * (vMax - vMin));
			setColourAndBrightness(t, rbb.maxX, rbb.maxZ);
			t.addVertexWithUV(aabb.maxX, aabb.maxY, aabb.maxZ, uMin + txmax * (uMax - uMin), vMin + tzmax * (vMax - vMin));
			setColourAndBrightness(t, rbb.maxX, rbb.minZ);
			t.addVertexWithUV(aabb.maxX, aabb.maxY, aabb.minZ, uMin + txmax * (uMax - uMin), vMin + tzmin * (vMax - vMin));
		}

		if (txmax < DRFMAX || !dontRenderFaces[Dir.PX]) {
			setTex(Dir.PX);
			t.setNormal(1, 0, 0);
			colNN = colPN = colNP = colPP = rgb(colourR * 0.6, colourG * 0.6, colourB * 0.6);
			getBrightnessXFace(rb, bx, by, bz, rbb.maxX);
			setColourAndBrightness(t, rbb.maxY, rbb.minZ);
			t.addVertexWithUV(aabb.maxX, aabb.maxY, aabb.minZ, uMax + tzmin * (uMin - uMax), vMax + tymax * (vMin - vMax));
			setColourAndBrightness(t, rbb.maxY, rbb.maxZ);
			t.addVertexWithUV(aabb.maxX, aabb.maxY, aabb.maxZ, uMax + tzmax * (uMin - uMax), vMax + tymax * (vMin - vMax));
			setColourAndBrightness(t, rbb.minY, rbb.maxZ);
			t.addVertexWithUV(aabb.maxX, aabb.minY, aabb.maxZ, uMax + tzmax * (uMin - uMax), vMax + tymin * (vMin - vMax));
			setColourAndBrightness(t, rbb.minY, rbb.minZ);
			t.addVertexWithUV(aabb.maxX, aabb.minY, aabb.minZ, uMax + tzmin * (uMin - uMax), vMax + tymin * (vMin - vMax));
		}
	}

	// renders centred on 0.5,0.5,0.5
	@Override
	@SideOnly(Side.CLIENT)
	public void renderPartInv(RenderBlocks render, ItemStack stack) {
		GL11.glDisable(GL11.GL_BLEND);
		GL11.glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
		GL11.glPushMatrix();

		EnumPosition pos;
		switch (clazz) {
		case Centre:
			pos = EnumPosition.Centre;
			break;
		case Panel:
		case HollowPanel:
			pos = EnumPosition.FaceNZ;
			GL11.glTranslatef(0, 0, 0.5f - (float) size / 2);
			break;
		case Strip:
			pos = EnumPosition.EdgeNXNZ;
			GL11.glTranslatef(0.5f - (float) size / 2, 0, 0.5f - (float) size / 2);
			break;
		case Corner:
			pos = EnumPosition.CornerNXNYNZ;
			GL11.glTranslatef(0.5f - (float) size / 2, 0.5f - (float) size / 2, 0.5f - (float) size / 2);
			break;
		default:
			pos = EnumPosition.Centre;
		}

		Minecraft.getMinecraft().renderEngine.func_110577_a(TextureMap.field_110575_b);		overrideIcon = null;

		Tessellator t = Tessellator.instance;
		t.startDrawingQuads();

		renderQuads(t, pos);

		t.draw();
		GL11.glPopMatrix();
		GL11.glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
		GL11.glDisable(GL11.GL_BLEND);
	}

	private void renderAABB(RenderBlocks render, int x, int y, int z, double minx, double miny, double minz, double maxx, double maxy, double maxz, Block block, boolean[] dontRenderFaces) {
		renderAABB(Tessellator.instance, AxisAlignedBB.getAABBPool().getAABB(minx, miny, minz, maxx, maxy, maxz).offset(x, y, z), render, x, y, z, block, dontRenderFaces);
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void renderPartWorld(RenderBlocks render, Part p, int x, int y, int z, boolean[] dontRenderFaces) {
		overrideIcon = render.overrideBlockTexture;

		float i = p.pos.ordinal() / 100000.0f; // unnoticeable
												// position-dependent inset, to
												// reduce flickering

		if (clazz == EnumPartClass.HollowPanel) {
			float hes = 0.25f; // (float)HOLLOW_EDGE_SIZE;
			float s = (float) size;

			switch (p.pos) {
			case FaceNX:
				renderAABB(render, x, y, z, i, i, i, s, hes, 1 - i, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, i, 1 - hes, i, s, 1 - i, 1 - i, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, i, hes, i, s, 1 - hes, hes, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, i, hes, 1 - hes, s, 1 - hes, 1 - i, modelBlock, dontRenderFaces);
				break;
			case FacePX:
				renderAABB(render, x, y, z, 1 - s, i, i, 1 - i, hes, 1 - i, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, 1 - s, 1 - hes, i, 1 - i, 1 - i, 1 - i, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, 1 - s, hes, i, 1 - i, 1 - hes, hes, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, 1 - s, hes, 1 - hes, 1 - i, 1 - hes, 1 - i, modelBlock, dontRenderFaces);
				break;
			case FaceNY:
				renderAABB(render, x, y, z, i, i, i, hes, s, 1 - i, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, 1 - hes, i, i, 1 - i, s, 1 - i, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, hes, i, i, 1 - hes, s, hes, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, hes, i, 1 - hes, 1 - hes, s, 1 - i, modelBlock, dontRenderFaces);
				break;
			case FacePY:
				renderAABB(render, x, y, z, i, 1 - s, i, hes, 1 - i, 1 - i, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, 1 - hes, 1 - s, i, 1 - i, 1 - i, 1 - i, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, hes, 1 - s, i, 1 - hes, 1 - i, hes, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, hes, 1 - s, 1 - hes, 1 - hes, 1 - i, 1 - i, modelBlock, dontRenderFaces);
				break;
			case FaceNZ:
				renderAABB(render, x, y, z, i, i, i, hes, 1 - i, s, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, 1 - hes, i, i, 1 - i, 1 - i, s, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, hes, i, i, 1 - hes, hes, s, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, hes, 1 - hes, 0, 1 - hes, 1 - i, s, modelBlock, dontRenderFaces);
				break;
			case FacePZ:
				renderAABB(render, x, y, z, i, i, 1 - s, hes, 1 - i, 1 - i, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, 1 - hes, i, 1 - s, 1 - i, 1 - i, 1 - i, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, hes, i, 1 - s, 1 - hes, hes, 1 - i, modelBlock, dontRenderFaces);
				renderAABB(render, x, y, z, hes, 1 - hes, 1 - s, 1 - hes, 1 - i, 1 - i, modelBlock, dontRenderFaces);
				break;
			default:
				// shouldn't happen
				System.err.println("hollow panel placed at invalid position " + p.pos + " in block " + x + "," + y + "," + z);
			}
		} else {
			AxisAlignedBB bb = p.getBoundingBoxFromPool();
			if (bb.minX == 0)
				bb.minX = i;
			if (bb.minY == 0)
				bb.minY = i;
			if (bb.minZ == 0)
				bb.minZ = i;
			if (bb.maxX == 1)
				bb.maxX -= i;
			if (bb.maxY == 1)
				bb.maxY -= i;
			if (bb.maxZ == 1)
				bb.maxZ -= i;
			renderAABB(render, x, y, z, bb.minX, bb.minY, bb.minZ, bb.maxX, bb.maxY, bb.maxZ, modelBlock, dontRenderFaces);
		}
	}

	@Override
	public boolean isOpaque() {
		return modelBlock.isOpaqueCube();
	}
}
