package mrtjp.projectred.transportation;

import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;

public abstract class PipeLogic {

    public static PipeLogic createPipeLogic(BasicPipePart p, int meta) {
        if (meta == 0)
            return new BasicPipeLogic(p);
        else if (meta == 1)
            return new RoutedJunctionPipeLogic(p);
        else if (meta == 2)
            return new RoutedInterfacePipeLogic(p);
        else if (meta == 3)
            return new RoutedCraftingPipeLogic(p);
        else if (meta == 4)
            return new RoutedRequestPipeLogic(p);
        return null;
    }

    BasicPipePart p;

    public PipeLogic(BasicPipePart p) {
        this.p = p;
    }

    public void save(NBTTagCompound tag) {
    }

    public void load(NBTTagCompound tag) {
    }

    public void readDesc(MCDataInput packet) {
    }

    public void writeDesc(MCDataOutput packet) {
    }

    public void read(MCDataInput packet, int switch_key) {
    }

    public void tick() {
    }

    public abstract boolean endReached(RoutedPayload r);

    public abstract boolean centerReached(RoutedPayload r);

    public abstract boolean handleDrop(RoutedPayload r);

    public abstract boolean resolveDestination(RoutedPayload r);

    public abstract Icon getIcon(BasicPipePart p, int i);

    public static class BasicPipeLogic extends PipeLogic {

        public BasicPipeLogic(BasicPipePart p) {
            super(p);
        }

        @Override
        public boolean endReached(RoutedPayload r) {
            return false;
        }

        @Override
        public boolean centerReached(RoutedPayload r) {
            return false;
        }

        @Override
        public boolean resolveDestination(RoutedPayload r) {
            return false;
        }

        @Override
        public Icon getIcon(BasicPipePart p, int i) {
            return EnumPipe.BASIC.sprites[0];
        }

        @Override
        public boolean handleDrop(RoutedPayload r) {
            return false;
        }
    }

    public static class RoutedJunctionPipeLogic extends PipeLogic {

        public RoutedJunctionPipeLogic(BasicPipePart p) {
            super(p);
        }

        @Override
        public boolean endReached(RoutedPayload r) {
            return false;
        }

        @Override
        public boolean centerReached(RoutedPayload r) {
            return false;
        }

        @Override
        public boolean resolveDestination(RoutedPayload r) {
            return false;
        }

        @Override
        public Icon getIcon(BasicPipePart p, int i) {
            return EnumPipe.ROUTEDJUNCTION.sprites[0];
        }

        @Override
        public boolean handleDrop(RoutedPayload r) {
            return false;
        }
    }

    public static class RoutedInterfacePipeLogic extends RoutedJunctionPipeLogic {

        public RoutedInterfacePipeLogic(BasicPipePart p) {
            super(p);
        }

        @Override
        public Icon getIcon(BasicPipePart p, int i) {
            return EnumPipe.ROUTEDINTERFACE.sprites[0];
        }
    }

    public static class RoutedCraftingPipeLogic extends RoutedJunctionPipeLogic {

        public RoutedCraftingPipeLogic(BasicPipePart p) {
            super(p);
        }

        @Override
        public Icon getIcon(BasicPipePart p, int i) {
            return EnumPipe.ROUTEDCRAFTING.sprites[0];
        }
    }
    public static class RoutedRequestPipeLogic extends RoutedJunctionPipeLogic {

        public RoutedRequestPipeLogic(BasicPipePart p) {
            super(p);
        }

        @Override
        public Icon getIcon(BasicPipePart p, int i) {
            return EnumPipe.ROUTEDREQUEST.sprites[0];
        }
    }
}
