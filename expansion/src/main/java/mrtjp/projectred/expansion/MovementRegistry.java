package mrtjp.projectred.expansion;

import codechicken.multipart.block.TileMultipart;
import codechicken.multipart.init.CBMultipartModContent;
import mrtjp.projectred.api.*;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.chunk.LevelChunk;
import net.minecraft.world.level.chunk.LevelChunkSection;
import net.minecraft.world.level.levelgen.Heightmap;
import net.minecraftforge.common.util.LazyOptional;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class MovementRegistry {

    //TODO maybe these need to be ForgeRegistries to ensure clients get same mover tied to a block?
    private static final List<FrameInteraction> frameInteractions = new LinkedList<>();
    private static final Map<Block, BlockMover> blockMovers = new HashMap<>();

    public static void init() {
        // Register built-in movers
        blockMovers.put(CBMultipartModContent.MULTIPART_BLOCK.get(), MultipartTileMover.INSTANCE);
    }

    public static void registerBlockMover(Block block, BlockMover mover) {
        blockMovers.put(block, mover);
    }

    public static void registerFrameInteraction(FrameInteraction interaction) {
        frameInteractions.add(interaction);
    }

    public static Frame getFrame(Level level, BlockPos pos) {
        Block block = level.getBlockState(pos).getBlock();
        if (block instanceof Frame f) return f;

        BlockEntity be = level.getBlockEntity(pos);
        if (be instanceof Frame f) return f;

        if (be != null) {
            LazyOptional<Frame> cap = be.getCapability(IExpansionAPI.FRAME_CAPABILITY);
            if (cap.isPresent()) return cap.orElseThrow(() -> new RuntimeException("??"));
        }

        for (FrameInteraction interaction : frameInteractions) {
            if (interaction.canInteract(level, pos)) return interaction;
        }

        return null;
    }

    public static MovementController getMovementController(Level level, BlockPos pos) {
        Block block = level.getBlockState(pos).getBlock();
        if (block instanceof MovementController mhb) return mhb;

        BlockEntity be = level.getBlockEntity(pos);
        if (be instanceof MovementController mbe) return mbe;

        return null;
    }

    public static BlockMover getMover(Level level, BlockPos pos) {

        Block block = level.getBlockState(pos).getBlock();
        if (blockMovers.containsKey(block)) {
            return blockMovers.get(block);
        }

        // Default and most compatible mover
        return SaveLoadTileMover.INSTANCE;
    }

    private static class MultipartTileMover implements BlockMover {

        public static final MultipartTileMover INSTANCE = new MultipartTileMover();

        private MultipartTileMover() {
        }

        @Override
        public boolean canMove(Level w, BlockPos pos) {
            return w.getBlockEntity(pos) instanceof TileMultipart;
        }

        @Override
        public void move(Level w, BlockPos pos, Direction dir) {
            BlockEntity be = w.getBlockEntity(pos);
            if (!(be instanceof TileMultipart tmp)) return;

            LevelChunk chunk = w.getChunkAt(pos);
            BlockState state = w.getBlockState(pos);
            BlockPos pos2 = pos.relative(dir);

            // Remove old block and tile
            silentSetBlockState(chunk, pos, Blocks.AIR.defaultBlockState());
            chunk.removeBlockEntity(pos);

            // Set blockstate in new position without causing blockentity creation
            silentSetBlockState(chunk, pos2, state);

            // Move the tile and add it back
            tmp.worldPosition = pos2;
            chunk.addAndRegisterBlockEntity(tmp);
        }

        @Override
        public void postMove(Level w, BlockPos pos) {
            BlockEntity be = w.getBlockEntity(pos);
            if (be instanceof TileMultipart tmp) {
                tmp.onMoved();
            }
        }
    }

    private static class SaveLoadTileMover implements BlockMover {

        public static final SaveLoadTileMover INSTANCE = new SaveLoadTileMover();

        private SaveLoadTileMover() {
        }

        @Override
        public boolean canMove(Level w, BlockPos pos) {
            // This is the default mover that can handle any block
            return true;
        }

        @Override
        public void move(Level w, BlockPos pos, Direction dir) {
            LevelChunk chunk = w.getChunkAt(pos);
            BlockPos pos2 = pos.relative(dir);

            BlockState state = w.getBlockState(pos); //ok, doesnt alert anything
            CompoundTag tag = chunk.getBlockEntityNbtForSaving(pos); // Save existing tile to nbt

            // Remove old block and tile
            silentSetBlockState(chunk, pos, Blocks.AIR.defaultBlockState());
            chunk.removeBlockEntity(pos);

            // Set blockstate in new position without causing blockentity creation
            silentSetBlockState(chunk, pos2, state);

            if (tag != null) {
                // Alter the tag into new position
                tag.putInt("x", pos2.getX());
                tag.putInt("y", pos2.getY());
                tag.putInt("z", pos2.getZ());
                // Add the tile back as pending
                chunk.setBlockEntityNbt(tag);
            }
        }

        @Override
        public void postMove(Level w, BlockPos pos) {
            LevelChunk chunk = w.getChunkAt(pos);

            // Force block entity promotion
            // TODO is there any benefit to doing this post-move?
            chunk.getBlockEntity(pos);
        }
    }

    // This should be identical to LevelChunk#setBlockState without block entity creation and state onRemove/onPlace notifications
    private static void silentSetBlockState(LevelChunk chunk, BlockPos pos, BlockState state) {

        int y = pos.getY();
        LevelChunkSection section = chunk.getSection(chunk.getSectionIndex(y));

        boolean hasOnlyAir = section.hasOnlyAir();
        if (hasOnlyAir && state.isAir()) return;

        int j = pos.getX() & 15;
        int k = y & 15;
        int l = pos.getZ() & 15;
        BlockState oldState = section.setBlockState(j, k, l, state);
        if (oldState == state) return;

        chunk.getOrCreateHeightmapUnprimed(Heightmap.Types.MOTION_BLOCKING).update(j, y, l, state);
        chunk.getOrCreateHeightmapUnprimed(Heightmap.Types.MOTION_BLOCKING_NO_LEAVES).update(j, y, l, state);
        chunk.getOrCreateHeightmapUnprimed(Heightmap.Types.OCEAN_FLOOR).update(j, y, l, state);
        chunk.getOrCreateHeightmapUnprimed(Heightmap.Types.WORLD_SURFACE).update(j, y, l, state);
    }
}
