# Instruction sets for ComputerCraft patching

list getBundledPowerOutput #int (world, x, y, z, side)
ILOAD 3
IFLT LRET #if (y < 0)
ILOAD 3
ALOAD 1
INVOKEVIRTUAL net/minecraft/world/World.getHeight ()I
IF_ICMPGE LRET #if (y >= world.getWorldHeight)
GETSTATIC mrtjp/projectred/api/ProjectRedAPI.transmissionAPI : Lmrtjp/projectred/api/ITransmissionAPI;
ALOAD 1
ILOAD 2
ILOAD 3
ILOAD 4
ILOAD 5
INVOKEINTERFACE mrtjp/projectred/api/ITransmissionAPI.getBundledInput (Lnet/minecraft/world/World;IIII)[B
INVOKESTATIC mrtjp/projectred/transmission/BundledCableCommons.packDigital ([B)I
IRETURN
LRET
ICONST_0
IRETURN

list cableUpdate #notify block neighbor change on bundled updates
ALOAD 3
ALOAD 0
GETFIELD dan200/computer/shared/NetworkedComputerHelper.m_owner : Lnet/minecraft/tileentity/TileEntity;
GETFIELD net/minecraft/tileentity/TileEntity.xCoord : I
GETSTATIC net/minecraft/util/Facing.offsetsXForSide : [I
ILOAD 6
IALOAD
IADD
ALOAD 0
GETFIELD dan200/computer/shared/NetworkedComputerHelper.m_owner : Lnet/minecraft/tileentity/TileEntity;
GETFIELD net/minecraft/tileentity/TileEntity.yCoord : I
GETSTATIC net/minecraft/util/Facing.offsetsYForSide : [I
ILOAD 6
IALOAD
IADD
ALOAD 0
GETFIELD dan200/computer/shared/NetworkedComputerHelper.m_owner : Lnet/minecraft/tileentity/TileEntity;
GETFIELD net/minecraft/tileentity/TileEntity.zCoord : I
GETSTATIC net/minecraft/util/Facing.offsetsZForSide : [I
ILOAD 6
IALOAD
IADD
ALOAD 0
GETFIELD dan200/computer/shared/NetworkedComputerHelper.m_block : Ldan200/computer/shared/BlockComputerBase;
GETFIELD dan200/computer/shared/BlockComputerBase.blockID : I
INVOKEVIRTUAL net/minecraft/world/World.notifyBlockOfNeighborChange (IIII)V

list firstFrame
ALOAD 1
ALOAD 0
GETFIELD dan200/computer/shared/NetworkedComputerHelper.m_owner : Lnet/minecraft/tileentity/TileEntity;
GETFIELD net/minecraft/tileentity/TileEntity.xCoord : I
ALOAD 0
GETFIELD dan200/computer/shared/NetworkedComputerHelper.m_owner : Lnet/minecraft/tileentity/TileEntity;
GETFIELD net/minecraft/tileentity/TileEntity.yCoord : I
ALOAD 0
GETFIELD dan200/computer/shared/NetworkedComputerHelper.m_owner : Lnet/minecraft/tileentity/TileEntity;
GETFIELD net/minecraft/tileentity/TileEntity.zCoord : I
ALOAD 0
GETFIELD dan200/computer/shared/NetworkedComputerHelper.m_block : Ldan200/computer/shared/BlockComputerBase;
GETFIELD dan200/computer/shared/BlockComputerBase.blockID : I
INVOKEVIRTUAL net/minecraft/world/World.notifyBlocksOfNeighborChange (IIII)V