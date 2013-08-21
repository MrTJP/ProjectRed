package mrtjp.projectred.integration;

import static mrtjp.projectred.transmission.BasicWireUtils.oldBACK;
import static mrtjp.projectred.transmission.BasicWireUtils.oldFRONT;
import static mrtjp.projectred.transmission.BasicWireUtils.oldLEFT;
import static mrtjp.projectred.transmission.BasicWireUtils.oldRIGHT;
import mrtjp.projectred.core.BasicRenderUtils;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.transmission.EnumWire;
import mrtjp.projectred.transmission.WirePart;
//import mrtjp.projectred.transmission.WireRenderAssistant;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.entity.player.EntityPlayer;

/**
 * GateRenderBridge provides a bridge between a gate's bit-masked render-state
 * to the renderer. Each time a rendering is checked, this gets a bit-mask from
 * the actual gate, and sets all the states of the torches, pointers, etc. Then,
 * the GateDynamicRenderer renders things that are common in all gates, like
 * torches and pointers. Then, this is asked if there is something it wants to
 * render special just for that gate, like bundled cabling or levers.
 * 
 * @author MrTJP
 * 
 */
public abstract class GateRenderBridge {
    // Wire colours
    public static final int OFF = 0x400000;
    public static final int ON = 0xFF0000;
    public static final int DISABLED = 0xC0C0C0;

    /**
     * These control which spots on a gate is a tiny square of wire.
     */
    public float[][] wirePosX = new float[][] { {} };
    public float[][] wirePosZ = new float[][] { {} };
    public int[] wireColor = new int[] {};

    /**
     * Torch positions are relative to the texture, in pixels. X+ goes right, Z+
     * goes down on the texture. Y changes the amount its pushed into the gate,
     * 0 by default.
     */
    public float[] torchX = new float[] {};
    public float[] torchY = new float[] {};
    public float[] torchZ = new float[] {};
    public boolean[] torchState = new boolean[] {};

    /**
     * Works the same as torch positions, except later, a pointer is rendered on
     * it.
     */
    public float[] pointerX = new float[] {};
    public float[] pointerY = new float[] {};
    public float[] pointerZ = new float[] {};

    // Part models
    public static RotatedPartModel _modelBase;
    public static RotatedPartModel _torchOff;
    public static RotatedPartModel _torchOn;
    public static RotatedPartModel _greenTorchOff;
    public static RotatedPartModel _greenTorchOn;
    public static RotatedPartModel _wire;
    public static RotatedPartModel _lever;
    public static RotatedPartModel _pointer;
    public static RotatedPartModel _chipYellow;
    public static RotatedPartModel _chipRed;
    public static RotatedPartModel _diode;
    public static RotatedPartModel _latchCableCover;
    public static RotatedPartModel _relayCableCover;
    public static RotatedPartModel _multCableCover;
    public static RotatedPartModel _solar;
    public static RotatedPartModel _rainSensor;
    public static RotatedPartModel _lightPanel;
    public static RotatedPartModel[] _panelLights = new RotatedPartModel[16];
    public static RotatedPartModel _serialBus;

    public static void registerAllIcons(IconRegister reg) {
        String baseTex = "projectred:gates/";
        _modelBase = new RotatedPartModel("gateparts/base.obj", reg.registerIcon(baseTex + "base"));
        _torchOff = new RotatedPartModel("gateparts/torch.obj", reg.registerIcon(baseTex + "redtorchoff"));
        _torchOn = new RotatedPartModel(_torchOff.getCCModels(), "gateparts/torch.obj", reg.registerIcon(baseTex + "redtorchon"));
        _greenTorchOff = new RotatedPartModel(_torchOff.getCCModels(), "gateparts/torch.obj", reg.registerIcon(baseTex + "greentorchoff"));
        _greenTorchOn = new RotatedPartModel(_torchOff.getCCModels(), "gateparts/torch.obj", reg.registerIcon(baseTex + "greentorchon"));
        _wire = new RotatedPartModel("gateparts/wire.obj", reg.registerIcon(baseTex + "wire"));
        _pointer = new RotatedPartModel("gateparts/pointer.obj", reg.registerIcon(baseTex + "pointer"));
        _lever = new RotatedPartModel("gateparts/lever.obj", reg.registerIcon(baseTex + "lever"));
        _chipYellow = new RotatedPartModel("gateparts/chip.obj", reg.registerIcon(baseTex + "yellowchip"));
        _chipRed = new RotatedPartModel(_chipYellow.getCCModels(), "gateparts/chip.obj", reg.registerIcon(baseTex + "redchip"));
        _diode = new RotatedPartModel("gateparts/diode.obj", reg.registerIcon(baseTex + "diode"));
        _latchCableCover = new RotatedPartModel("gateparts/cablecover.obj", reg.registerIcon(baseTex + "cablelatch"));
        _relayCableCover = new RotatedPartModel(_latchCableCover.getCCModels(), "gateparts/cablecover.obj", reg.registerIcon(baseTex + "cablerelay"));
        _multCableCover = new RotatedPartModel(_latchCableCover.getCCModels(), "gateparts/cablecover.obj", reg.registerIcon(baseTex + "cablemulti"));
        _solar = new RotatedPartModel("gateparts/solar.obj", reg.registerIcon(baseTex + "solar"));
        _rainSensor = new RotatedPartModel("gateparts/rainsensor.obj", reg.registerIcon(baseTex + "rainsensor"));
        _lightPanel = new RotatedPartModel("gateparts/lightpanel.obj", reg.registerIcon(baseTex + "lightpanel"));
        for (int i = 0; i < 16; i++) {
            _panelLights[i] = new RotatedPartModel(_lightPanel.getCCModels(), "gateparts/lightpanel.obj", reg.registerIcon(baseTex + "lightpanel/lightpanel" + i));
        }
        _serialBus = new RotatedPartModel("gateparts/serialbus.obj", reg.registerIcon(baseTex + "serialbus"));
    }

    /**
     * Used to prepare the gate for render based on the renderState bitmask
     * obtained from the gate.
     * 
     * @param renderState
     */
    public void set(int renderState) {
    }

    /**
     * Used to prepare the gate for inventory rendering.
     */
    public void setItemRender() {
    }

    /**
     * Used to render special objects that are not torches, wires, or pointers.
     * 
     * @param x
     * @param y
     * @param z
     * @param side
     * @param facing
     */
    public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
    }

    public static class Default extends GateRenderBridge implements Stateless {
    }

    /**
     * Marker interface for renderings that don't store any state. Similar to
     * GateLogic.Stateless. Currently not used.
     */
    public static interface Stateless {
    }

    /**
     * Dummy WirePart class, used to pass to WireRenderAssistant.
     */
    /*public static class WireRenderDummy extends WirePart {

        public WireRenderDummy() {
            super(EnumWire.RED_ALLOY, false, 0);
        }

        public boolean[] connects = new boolean[6];
        public boolean[] connectsCor = new boolean[6];
        public boolean[] connectsInt = new boolean[6];

        @Override
        public boolean getExternalConnectionOveride(int absDir) {
            return false;
        }

        @Override
        public boolean maskConnects(int absDir) {
            return connects[absDir];
        }

        @Override
        public boolean maskConnectsAroundCorner(int absDir) {
            return connectsCor[absDir];
        }

        @Override
        public boolean maskConnectsInternally(int absDir) {
            return connectsInt[absDir];
        }

        @Override
        protected boolean debug(EntityPlayer ply) {
            return false;
        }
    }*/

    public static class AND extends GateRenderBridge implements Stateless {
        {
            // Wires are {outCenter, back, right, left, outTop}
            wirePosX = new float[][] { { 4.5f, 4.5f, 5, 6, 7, 8, 8.5f, 9, 10, 11, 12, 12.5f, 12.5f }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 12.5f, 12.5f, 12.5f, 13, 14, 15 }, { 4.5f, 4.5f, 4.5f, 4, 3, 2 }, { 8.5f, 8.5f } };
            wirePosZ = new float[][] { { 6, 5, 5, 5, 5, 5, 6, 5, 5, 5, 5, 5, 6 }, { 7, 8, 9, 10, 11, 12, 13, 14, 15 }, { 7, 8, 8.5f, 8.5f, 8.5f, 8.5f }, { 7, 8, 8.5f, 8.5f, 8.5f, 8.5f }, { 2, 2.5f } };
            wireColor = new int[] { 0, 0, 0, 0, 0 };
            torchX = new float[] { 8.5f, 4.5f, 8.5f, 12.5f };
            torchY = new float[] { 0f, 0f, 0f, 0f };
            torchZ = new float[] { 2.5f, 7f, 7f, 7f };
            torchState = new boolean[] { false, true, true, true };
        }

        @Override
        public void set(int renderState) {
            boolean out_on = (renderState & 1) != 0;
            boolean back_on = (renderState & 2) != 0;
            boolean left_on = (renderState & 4) != 0;
            boolean right_on = (renderState & 8) != 0;
            boolean left_disabled = (renderState & 16) != 0;
            boolean back_disabled = (renderState & 32) != 0;
            boolean right_disabled = (renderState & 64) != 0;
            boolean out_in = (renderState & 128) != 0;

            wireColor[0] = out_on ? OFF : ON;
            wireColor[1] = back_disabled ? DISABLED : back_on ? ON : OFF;
            wireColor[2] = right_disabled ? DISABLED : right_on ? ON : OFF;
            wireColor[3] = left_disabled ? DISABLED : left_on ? ON : OFF;
            wireColor[4] = out_in ? ON : OFF;
            torchState[0] = out_on;
            torchState[1] = !left_on && !left_disabled;
            torchState[2] = !back_on && !back_disabled;
            torchState[3] = !right_on && !right_disabled;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = ON;
            wireColor[1] = OFF;
            wireColor[2] = OFF;
            wireColor[3] = OFF;
            torchState[0] = false;
            torchState[1] = true;
            torchState[2] = true;
            torchState[3] = true;
        }
    }

    public static class OR extends GateRenderBridge implements Stateless {
        {
            // Wires are {out, back, right, left, center}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 7.5f, 9.5f }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 15, 14, 13, 13, 13, 12, 11, 10, 9.5f }, { 2, 3, 4, 4, 4, 5, 6, 7, 7.5f }, { 8.5f, 8.5f, 8.5f, 7.5f, 9.5f } };
            wirePosZ = new float[][] { { 2, 3, 4, 4, 4 }, { 10, 11, 12, 13, 14, 15 }, { 8.5f, 8.5f, 8.5f, 9f, 9.5f, 9.5f, 9.5f, 9.5f, 9.5f }, { 8.5f, 8.5f, 8.5f, 9f, 9.5f, 9.5f, 9.5f, 9.5f, 9.5f }, { 7, 8, 9, 7, 7 } };
            wireColor = new int[] { 0, 0, 0, 0, 0 };
            torchX = new float[] { 8.5f, 8.5f };
            torchY = new float[] { 0f, 0f };
            torchZ = new float[] { 5.5f, 9.5f };
            torchState = new boolean[] { false, true };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 64) != 0 ? DISABLED : (renderState & 2) != 0 ? ON : OFF;
            wireColor[2] = (renderState & 128) != 0 ? DISABLED : (renderState & 8) != 0 ? ON : OFF;
            wireColor[3] = (renderState & 32) != 0 ? DISABLED : (renderState & 4) != 0 ? ON : OFF;
            torchState[0] = (renderState & 16) != 0;
            torchState[1] = !torchState[0];
            wireColor[4] = torchState[1] ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = OFF;
            wireColor[3] = OFF;
            wireColor[4] = ON;
            torchState[0] = false;
            torchState[1] = true;
        }
    }

    public static class NOT extends GateRenderBridge implements Stateless {
        {
            // Wires are {out, back, right, left}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 7.5f, 9.5f }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 15, 14, 13, 13, 13, 12.5f, 12.5f, 12.5f, }, { 2, 3, 4, 4, 4, 4.5f, 4.5f, 4.5f }, };
            wirePosZ = new float[][] { { 2, 3, 4, 4, 4 }, { 8.5f, 9, 10, 11, 12, 13, 14, 15 }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 7.5f, 9.5f }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 7.5f, 9.5f }, };
            wireColor = new int[] { 0, 0, 0, 0 };
            torchX = new float[] { 8.5f };
            torchY = new float[] { 0f };
            torchZ = new float[] { 8.5f };
            torchState = new boolean[] { false };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 64) != 0 ? DISABLED : (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 2) != 0 ? ON : OFF;
            wireColor[2] = (renderState & 128) != 0 ? DISABLED : (renderState & 8) != 0 ? ON : OFF;
            wireColor[3] = (renderState & 32) != 0 ? DISABLED : (renderState & 4) != 0 ? ON : OFF;
            torchState[0] = (renderState & 16) != 0;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = wireColor[2] = wireColor[3] = ON;
            wireColor[1] = OFF;
            torchState[0] = true;
        }
    }

    public static class RSLatch extends GateRenderBridge implements Stateless {
        {
            // Wires are {left, right}
            wirePosX = new float[][] { { 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 6, 7, 8, 8.5f, 8.5f, 8.5f, 8.5f, 2, 3 }, { 8.5f, 8.5f, 8.5f, 8.5f, 9, 10, 11, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 12, 14, 15 } };
            wirePosZ = new float[][] { { 13, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 4, 4, 4, 4, 4, 5, 3, 2, 8.5f, 8.5f }, { 15, 14, 12, 13, 13, 13, 13, 13, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 4, 8.5f, 8.5f } };
            wireColor = new int[] { 0, 0 };
            torchX = new float[] { 11f, 6f };
            torchY = new float[] { 0f, 0f };
            torchZ = new float[] { 4f, 13f };
            torchState = new boolean[] { false, false };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 2) != 0 ? ON : OFF;
            torchState[0] = (renderState & 4) != 0;
            torchState[1] = (renderState & 8) != 0;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = ON;
            wireColor[1] = OFF;
            torchState[0] = true;
            torchState[1] = false;
        }
    }

    public static class ToggleLatch extends GateRenderBridge implements Stateless {
        {
            // Wires are {left, right, top, bottom}
            wirePosX = new float[][] { { 2, 3, 4, 5, 6, 7, 8 }, { 15, 14, 13 }, { 4, 5, 6, 7, 8, 9, 10, 10, 8.5f, 8.5f }, { 4, 5, 6, 7, 8, 9, 10, 10, 8.5f, 8.5f } };
            wirePosZ = new float[][] { { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 8.5f, 8.5f, 8.5f }, { 3.5f, 3.5f, 3.5f, 3.5f, 3.5f, 3.5f, 3.5f, 4.5f, 2, 3 }, { 13.5f, 13.5f, 13.5f, 13.5f, 13.5f, 13.5f, 13.5f, 13, 15, 14 } };
            wireColor = new int[] { 0, 0, 0, 0 };
            torchX = new float[] { 4f, 4f };
            torchY = new float[] { 0f, 0f };
            torchZ = new float[] { 3.5f, 13.5f };
            torchState = new boolean[] { true, false };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 2) != 0 ? ON : OFF;
            torchState[0] = (renderState & 4) != 0;
            torchState[1] = (renderState & 8) != 0;
            wireColor[2] = torchState[0] ? ON : OFF;
            wireColor[3] = torchState[1] ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = ON;
            wireColor[3] = OFF;
            torchState[0] = true;
            torchState[1] = false;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR)
                return;
            // Lever model is 6x8
            float xPos = (16f - 10f) / 16f + .03f;
            float zPos = (16f - 8.5f) / 16f + .03f;
            rt.renderPartModel(_lever, "base", xPos, 2 / 16f, zPos, -1, -1, false);
            rt.renderPartModel(_lever, (torchState[0] ? "leveron" : "leveroff"), xPos, 2 / 16f, zPos, -1, -1, false);
        }
    }

    public static class NOR extends GateRenderBridge implements Stateless {
        {
            // Wires are {out, back, right, left}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 15, 14, 13, 13, 13, 12, 11, 10, 9.5f }, { 2, 3, 4, 4, 4, 5, 6, 7, 7.5f } };
            wirePosZ = new float[][] { { 2, 3, 4, 5, 6, 7 }, { 8, 9, 10, 11, 12, 13, 14, 15 }, { 8.5f, 8.5f, 8.5f, 8f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f }, { 8.5f, 8.5f, 8.5f, 8f, 7.5f, 7.5f, 7.5f, 7.5f, 7.5f } };
            wireColor = new int[] { 0, 0, 0, 0 };
            torchX = new float[] { 8.5f };
            torchY = new float[] { 0f };
            torchZ = new float[] { 7.5f };
            torchState = new boolean[] { false };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 64) != 0 ? DISABLED : (renderState & 2) != 0 ? ON : OFF;
            wireColor[2] = (renderState & 128) != 0 ? DISABLED : (renderState & 8) != 0 ? ON : OFF;
            wireColor[3] = (renderState & 32) != 0 ? DISABLED : (renderState & 4) != 0 ? ON : OFF;
            torchState[0] = (renderState & 16) != 0;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = wireColor[2] = wireColor[3] = ON;
            wireColor[1] = OFF;
            torchState[0] = true;
        }
    }

    public static class NAND extends GateRenderBridge implements Stateless {
        {
            // Wires are {out, back, left, right}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 4.5f, 4.5f, 5, 6, 7, 8, 8.5f, 9, 10, 11, 12, 12.5f, 12.5f }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 12.5f, 12.5f, 12.5f, 13, 14, 15 }, { 4.5f, 4.5f, 4.5f, 4, 3, 2 } };
            wirePosZ = new float[][] { { 2, 3, 4, 6, 5, 5, 5, 5, 5, 6, 5, 5, 5, 5, 5, 6 }, { 7, 8, 9, 10, 11, 12, 13, 14, 15 }, { 7, 8, 8.5f, 8.5f, 8.5f, 8.5f }, { 7, 8, 8.5f, 8.5f, 8.5f, 8.5f } };
            wireColor = new int[] { 0, 0, 0, 0 };
            torchX = new float[] { 4.5f, 8.5f, 12.5f };
            torchY = new float[] { 0f, 0f, 0f };
            torchZ = new float[] { 6.5f, 6.5f, 6.5f };
            torchState = new boolean[] { true, true, true };
        }

        @Override
        public void set(int renderState) {
            boolean left_disabled = (renderState & 16) != 0;
            boolean back_disabled = (renderState & 32) != 0;
            boolean right_disabled = (renderState & 64) != 0;
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = back_disabled ? DISABLED : (renderState & 2) != 0 ? ON : OFF;
            wireColor[2] = right_disabled ? DISABLED : (renderState & 8) != 0 ? ON : OFF;
            wireColor[3] = left_disabled ? DISABLED : (renderState & 4) != 0 ? ON : OFF;
            torchState[0] = (renderState & 4) == 0 && !left_disabled;
            torchState[1] = (renderState & 2) == 0 && !back_disabled;
            torchState[2] = (renderState & 8) == 0 && !right_disabled;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = ON;
            wireColor[1] = OFF;
            wireColor[2] = OFF;
            wireColor[3] = OFF;
            torchState[0] = true;
            torchState[1] = true;
            torchState[2] = true;
        }
    }

    public static class XOR extends GateRenderBridge implements Stateless {
        {
            // Wires are {left, right, center, out}
            wirePosX = new float[][] { { 2, 3, 4, 3, 3, 3, 3, 3, 4, 5, 6, 7, 8 }, { 15, 14, 13, 14, 14, 14, 14, 14, 13, 12, 11, 10, 9 }, { 5.5f, 6.5f, 7.5f, 9.5f, 10.5f, 11.5f, 8.5f, 8.5f, 8.5f, 8.5f, 7.5f, 9.5f }, { 8.5f, 8.5f, 8.5f, 7.5f, 6.5f, 5.5f, 4.5f, 9.5f, 10.5f, 11.5f, 12.5f, 4.5f, 4.5f, 4.5f, 4.5f, 12.5f, 12.5f, 12.5f, 12.5f } };
            wirePosZ = new float[][] { { 8.5f, 8.5f, 8.5f, 9.5f, 10.5f, 11.5f, 12.5f, 13.5f, 13.5f, 13.5f, 13.5f, 13.5f, 13.5f, }, { 8.5f, 8.5f, 8.5f, 9.5f, 10.5f, 11.5f, 12.5f, 13.5f, 13.5f, 13.5f, 13.5f, 13.5f, 13.5f, }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 9.5f, 10.5f, 11, 11, 11 }, { 2, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 6, 7, 8, 5, 6, 7, 8 } };
            wireColor = new int[] { 0, 0, 0, 0 };
            torchX = new float[] { 4.5f, 12.5f, 8.5f };
            torchY = new float[] { 0f, 0f, 0f };
            torchZ = new float[] { 8.5f, 8.5f, 13.5f };
            torchState = new boolean[] { false, false, false };
        }

        @Override
        public void set(int renderState) {
            boolean left = (renderState & 1) != 0;
            boolean right = (renderState & 2) != 0;
            boolean out = (renderState & 4) != 0;
            wireColor[0] = left ? ON : OFF;
            wireColor[1] = right ? ON : OFF;
            wireColor[2] = !left && !right ? ON : OFF;
            wireColor[3] = out ? ON : OFF;
            torchState[0] = !left && right;
            torchState[1] = left && !right;
            torchState[2] = !left && !right;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = ON;
            wireColor[3] = OFF;
            torchState[0] = false;
            torchState[1] = false;
            torchState[2] = true;
        }
    }

    public static class XNOR extends GateRenderBridge implements Stateless {
        {
            // Wires are {leftIn, rightIn, center, leftOut, rightOut, out}
            wirePosX = new float[][] { { 2, 3, 4, 3, 3, 3, 3, 3, 4, 5, 6, 7, 8 }, { 15, 14, 13, 14, 14, 14, 14, 14, 13, 12, 11, 10, 9 }, { 5.5f, 6.5f, 7.5f, 9.5f, 10.5f, 11.5f, 8.5f, 8.5f, 8.5f, 8.5f, 7.5f, 9.5f }, { 7.5f, 6.5f, 5.5f, 4.5f, 4.5f, 4.5f, 4.5f, 4.5f, 4.5f, }, { 9.5f, 10.5f, 11.5f, 12.5f, 12.5f, 12.5f, 12.5f, 12.5f, 12.5f }, { 8.5f }, };
            wirePosZ = new float[][] { { 8.5f, 8.5f, 8.5f, 9.5f, 10.5f, 11.5f, 12.5f, 13.5f, 13.5f, 13.5f, 13.5f, 13.5f, 13.5f, }, { 8.5f, 8.5f, 8.5f, 9.5f, 10.5f, 11.5f, 12.5f, 13.5f, 13.5f, 13.5f, 13.5f, 13.5f, 13.5f, }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 9.5f, 10.5f, 11, 11, 11 }, { 3, 3, 3, 3, 4, 5, 6, 7, 8 }, { 3, 3, 3, 3, 4, 5, 6, 7, 8 }, { 2 } };
            wireColor = new int[] { 0, 0, 0, 0, 0, 0 };
            torchX = new float[] { 8.5f, 4.5f, 12.5f, 8.5f };
            torchY = new float[] { 0f, 0f, 0f, 0f };
            torchZ = new float[] { 3.0f, 8.5f, 8.5f, 13.5f };
            torchState = new boolean[] { false, false, false, false };
        }

        @Override
        public void set(int renderState) {
            boolean left = (renderState & 1) != 0;
            boolean right = (renderState & 2) != 0;
            boolean out = (renderState & 4) != 0;
            wireColor[0] = left ? ON : OFF;
            wireColor[1] = right ? ON : OFF;
            wireColor[2] = !left && !right ? ON : OFF;
            wireColor[3] = !left && right ? ON : OFF;
            wireColor[4] = left && !right ? ON : OFF;
            wireColor[5] = (renderState & 8) != 0 ? ON : OFF;
            torchState[0] = out;
            torchState[1] = !left && right;
            torchState[2] = left && !right;
            torchState[3] = !left && !right;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = ON;
            wireColor[3] = OFF;
            wireColor[4] = OFF;
            wireColor[5] = ON;
            torchState[0] = true;
            torchState[1] = false;
            torchState[2] = false;
            torchState[3] = true;
        }
    }

    public static class Buffer extends GateRenderBridge implements Stateless {
        {
            // Wires are {left, right, back, center, out}
            wirePosX = new float[][] { { 2, 3, 4, 4, 4, 4, 4, 4, 5, 6, 6, 6 }, { 15, 14, 13, 13, 13, 13, 13, 13, 12, 11, 11, 11, }, { 8.5f, 8.5f, 8.5f, }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 7.5f, 9.5f, }, { 8.5f, 8.5f } };
            wirePosZ = new float[][] { { 8.5f, 8.5f, 8.5f, 7.5f, 6.5f, 5.5f, 4.5f, 4, 4, 4, 5, 3 }, { 8.5f, 8.5f, 8.5f, 7.5f, 6.5f, 5.5f, 4.5f, 4, 4, 4, 5, 3 }, { 15, 14, 13 }, { 5, 6, 7, 8, 9, 10, 10, 10, }, { 2, 3 } };
            wireColor = new int[] { 0, 0, 0, 0, 0 };
            torchX = new float[] { 8.5f, 8.5f };
            torchY = new float[] { 0f, 0f };
            torchZ = new float[] { 4f, 12f };
            torchState = new boolean[] { false, false };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 4) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 8) != 0 ? ON : OFF;
            wireColor[2] = (renderState & 2) != 0 ? ON : OFF;
            wireColor[3] = (renderState & 2) == 0 ? ON : OFF;
            wireColor[4] = (renderState & 16) != 0 ? ON : OFF;
            torchState[0] = (renderState & 1) != 0;
            torchState[1] = (renderState & 2) == 0;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = OFF;
            wireColor[3] = ON;
            wireColor[4] = ON;
            torchState[0] = false;
            torchState[1] = true;
        }
    }

    public static class Multiplexer extends GateRenderBridge implements Stateless {
        {
            // Wires are {inputBack, inputLeft, inputRight, topRight, topLeft,
            // bottomLeft, outputTop}
            wirePosX = new float[][] { { 8.5f, 8.5f, 7.5f, 6.5f, 5.5f, 4.5f, 8.5f, 9.5f, 10.5f, 11.5f, 12.5f, 12.5f, 12.5f, 12.5f, 12.5f, }, { 2, 3, 4 }, { 15, 14, 13 }, { 3.5f, 4.5f, 5.5f, 4.5f, 4.5f, 4.5f }, { 7.5f, 6.5f, 5.5f, 4.5f, 4.5f, 4.5f, 4.5f, 3.5f, 5.5f }, { 9.5f, 10.5f, 11.5f, 12.5f, 12.5f, 12.5f, 12.5f, 11.5f, 13.5f }, { 8.5f } };
            wirePosZ = new float[][] { { 15, 14, 14, 14, 14, 14, 13, 13, 13, 13, 13, 12, 11, 10, 9 }, { 8.5f, 8.5f, 8.5f }, { 8.5f, 8.5f, 8.5f }, { 12, 12, 12, 11, 10, 9 }, { 3, 3, 3, 3, 4, 5, 6f, 6f, 6f }, { 3, 3, 3, 3, 4, 5, 6f, 6f, 6f }, { 2 } };
            wireColor = new int[] { 0, 0, 0, 0, 0, 0, 0 };
            torchX = new float[] { 8.5f, 4.5f, 12.5f, 4.5f };
            torchY = new float[] { 0f, 0f, 0f, 0f };
            torchZ = new float[] { 3f, 8.5f, 8.5f, 14f };
            torchState = new boolean[] { false, false, false, false };
        }

        @Override
        public void set(int renderState) {
            boolean back = (renderState & 1) != 0;
            boolean left = (renderState & 2) != 0;
            boolean right = (renderState & 4) != 0;
            boolean out = (renderState & 8) != 0;
            wireColor[0] = back ? ON : OFF;
            wireColor[1] = left ? ON : OFF;
            wireColor[2] = right ? ON : OFF;
            wireColor[3] = !back ? ON : OFF;
            wireColor[4] = !left && back ? ON : OFF;
            wireColor[5] = !right && !back ? ON : OFF;
            wireColor[6] = (renderState & 16) != 0 ? ON : OFF;
            torchState[0] = out;
            torchState[1] = !left && back;
            torchState[2] = !right && !back;
            torchState[3] = !back;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = OFF;
            wireColor[3] = ON;
            wireColor[4] = OFF;
            wireColor[5] = ON;
            wireColor[6] = ON;
            torchState[0] = false;
            torchState[1] = false;
            torchState[2] = true;
            torchState[3] = true;
        }
    }

    public static class Repeater extends GateRenderBridge implements Stateless {
        {
            // Wires are {input, output}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.75f, 8.25f }, { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9 } };
            wirePosZ = new float[][] { { 14, 15, 13, 13 }, { 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 } };
            wireColor = new int[] { 0, 0 };
            torchX = new float[] { 8.5f, 8.5f };
            torchY = new float[] { 0f, 0f };
            torchZ = new float[] { 3f, 6f };
            torchState = new boolean[] { false, false };
        }

        @Override
        public void set(int renderState) {
            boolean out = (renderState & 32768) != 0;
            boolean in = (renderState & 64) != 0;
            torchZ[1] = (renderState & 7) + 6;
            torchState[0] = torchState[1] = out;
            wireColor[0] = in ? ON : OFF;
            wireColor[1] = out ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            torchZ[1] = 6f;
            torchState[0] = torchState[1] = false;
            wireColor[0] = ON;
            wireColor[1] = OFF;
        }
    }

    public static class Timer extends GateRenderBridge implements Stateless {
        {
            // Wires are {left, back, right, out}
            wirePosX = new float[][] { { 2, 3, 4, 4, 4, 4, 4, 4, 5, 6, 6, 6 }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 15, 14, 13, 13, 13, 13, 13, 13, 12, 11, 11, 11, }, { 8.5f, 8.5f, 8.5f } };
            wirePosZ = new float[][] { { 8.5f, 8.5f, 8.5f, 7.5f, 6.5f, 5.5f, 4.5f, 4, 4, 4, 5, 3 }, { 15, 14, 13, 12, 11, 10, 9, 8.5f }, { 8.5f, 8.5f, 8.5f, 7.5f, 6.5f, 5.5f, 4.5f, 4, 4, 4, 5, 3 }, { 2, 3, 4 } };
            wireColor = new int[] { 0, 0, 0, 0 };
            torchX = new float[] { 8.5f };
            torchY = new float[] { 0f };
            torchZ = new float[] { 4f };
            torchState = new boolean[] { false };
            pointerX = new float[] { 8.5f };
            pointerY = new float[] { 0f };
            pointerZ = new float[] { 8.5f };
        }

        @Override
        public void set(int renderState) {
            torchState[0] = (renderState & 1) != 0;
            wireColor[0] = (renderState & 2) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 4) != 0 ? ON : OFF;
            wireColor[2] = (renderState & 8) != 0 ? ON : OFF;
            wireColor[3] = (renderState & 32) != 0 ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            torchState[0] = false;
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = OFF;
            wireColor[3] = OFF;
        }
    }

    public static class Counter extends GateRenderBridge implements Stateless {
        {
            // Wires are {front, back, leftout, rightout}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 3, 4, 5 }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 12, 13, 13, 13, 14 }, { 2, 3, 4, }, { 15, 14, 13 } };
            wirePosZ = new float[][] { { 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 4, 4, 4 }, { 15, 14, 13, 12, 11, 13, 12, 13, 14, 13 }, { 8.5f, 8.5f, 8.5f }, { 8.5f, 8.5f, 8.5f } };
            wireColor = new int[] { 0, 0, 0, 0 };
            torchX = new float[] { 4f, 13f };
            torchY = new float[] { 0f, 0f };
            torchZ = new float[] { 8.5f, 8.5f };
            torchState = new boolean[] { false, false };
            pointerX = new float[] { 8.5f };
            pointerY = new float[] { 0f };
            pointerZ = new float[] { 11f };
        }

        @Override
        public void set(int renderState) {
            torchState[0] = (renderState & 4) != 0;
            torchState[1] = (renderState & 8) != 0;
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 2) != 0 ? ON : OFF;
            wireColor[2] = (renderState & 16) != 0 ? ON : OFF;
            wireColor[3] = (renderState & 32) != 0 ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            torchState[0] = true;
            torchState[1] = false;
        }
    }

    public static class Sequencer extends GateRenderBridge implements Stateless {
        {
            wireColor = new int[] {};
            torchX = new float[] { 8.5f, 3f, 14f, 8.5f };
            torchY = new float[] { 0f, 0f, 0f, 0f };
            torchZ = new float[] { 3f, 8.5f, 8.5f, 14f };
            torchState = new boolean[] { false, false, false, false };
            pointerX = new float[] { 8f };
            pointerY = new float[] { 0f };
            pointerZ = new float[] { 8f };
        }

        @Override
        public void set(int renderState) {
            torchState[0] = renderState == 0;
            torchState[1] = renderState == 3;
            torchState[2] = renderState == 1;
            torchState[3] = renderState == 2;
        }

        @Override
        public void setItemRender() {
            torchState[0] = true;
            torchState[1] = false;
            torchState[2] = false;
            torchState[3] = false;
        }
    }

    public static class PulseFormer extends GateRenderBridge implements Stateless {
        {
            // Wires are {inBack, topleft, center, topright, delayer, out}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 7.5f, 6.5f, 5.5f, 4.5f, 4.5f, 4.5f, 4.5f, 4.5f, }, { 5.5f, 3.5f, 4.5f, 4.5f, 4.5f, 4.5f, 5.5f, 6.5f, 7.5f, 8.5f }, { 6.5f, 6.5f, 6.5f, 7.5f, 8.5f, 9.5f, 10.5f, 11.5f, 12.5f }, { 12.5f, 12.5f, 12.5f, 12.25f, 12.75f }, { 12, 13, 12, 13, 12, 13, 11, 10, 9, 8.5f }, { 8.5f, 8.5f } };
            wirePosZ = new float[][] { { 15, 14, 13, 13, 13, 13, 13, 12, 11, 10, 9, 8.5f }, { 6, 6, 6, 5, 4, 3, 3, 3, 3, 3 }, { 7.5f, 8.5f, 9.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 7.5f, 6.5f, 5.5f, 5.5f, 5.5f }, { 4.5f, 4.5f, 3.5f, 3.5f, 3f, 3f, 3, 3, 3, 3 }, { 2, 3 } };
            wireColor = new int[] { 0, 0, 0, 0, 0, 0 };
            torchX = new float[] { 8.5f, 4.5f, 12.5f };
            torchY = new float[] { 0f, 0f, 0f };
            torchZ = new float[] { 3f, 8.5f, 8.5f };
            torchState = new boolean[] { false, false, false };
        }

        @Override
        public void set(int renderState) {
            boolean inBack = (renderState & 1) != 0;
            boolean out = (renderState & 2) != 0;
            boolean changing = (renderState & 4) != 0;
            torchState[0] = out;
            torchState[1] = !inBack;
            torchState[2] = inBack && (!out || changing);
            wireColor[0] = inBack ? ON : OFF;
            wireColor[1] = inBack ? OFF : ON;
            wireColor[2] = inBack ? OFF : ON;
            wireColor[3] = torchState[2] ? ON : OFF;
            wireColor[4] = !torchState[0] && torchState[2] && !changing ? ON : OFF;
            wireColor[5] = (renderState & 8) != 0 ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            torchState[0] = false;
            torchState[1] = true;
            torchState[2] = false;
            wireColor[0] = OFF;
            wireColor[1] = ON;
            wireColor[2] = ON;
            wireColor[3] = OFF;
            wireColor[4] = OFF;
            wireColor[5] = OFF;
        }
    }

    public static class Randomizer extends GateRenderBridge {
        {
            // Wires are {back, left, right, top}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 9.5f, 7.5f }, { 2, 3, 4 }, { 15, 14, 13 }, { 8.5f, 8.5f, 8.5f, 8.5f } };
            wirePosZ = new float[][] { { 15, 14, 13, 12, 11, 10, 9, 8, 7, 8.5f, 8.5f }, { 8.5f, 8.5f, 8.5f, }, { 8.5f, 8.5f, 8.5f, }, { 2, 3, 4, 5 } };
            wireColor = new int[] { 0, 0, 0, 0 };
            torchX = new float[] { 5.5f, 11.5f, 8.5f };
            torchY = new float[] { 0f, 0f, 0f };
            torchZ = new float[] { 8.5f, 8.5f, 5.5f };
        }

        // Chips are {left, right, top}
        boolean[] chipState = new boolean[] { false, false, false };

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 32) != 0 ? ON : OFF;
            wireColor[2] = (renderState & 64) != 0 ? ON : OFF;
            wireColor[3] = (renderState & 16) != 0 ? ON : OFF;
            chipState[0] = (renderState & 2) != 0;
            chipState[1] = (renderState & 4) != 0;
            chipState[2] = (renderState & 8) != 0;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = OFF;
            wireColor[3] = OFF;
            chipState[0] = false;
            chipState[1] = false;
            chipState[2] = false;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR) {
                return;
            }
            // Render left chip
            rt.renderPartModel(_chipYellow, (chipState[0] ? "diodeon" : "diodeoff"), (16f - torchX[0]) / 16f + .03f, torchY[0] / 16f, (16f - torchZ[0]) / 16f + .03f, -1, -1, false);
            rt.renderPartModel(_chipYellow, "base", (16f - torchX[0]) / 16f + .03f, torchY[0] / 16f, (16f - torchZ[0]) / 16f + .03f, -1, -1, false);

            // Render right chip
            rt.renderPartModel(_chipYellow, (chipState[1] ? "diodeon" : "diodeoff"), (16f - torchX[1]) / 16f + .03f, torchY[1] / 16f, (16f - torchZ[1]) / 16f + .03f, -1, -1, false);
            rt.renderPartModel(_chipYellow, "base", (16f - torchX[1]) / 16f + .03f, torchY[1] / 16f, (16f - torchZ[1]) / 16f + .03f, -1, -1, false);

            // Render top chip
            rt.renderPartModel(_chipYellow, (chipState[2] ? "diodeon" : "diodeoff"), (16f - torchX[2]) / 16f + .03f, torchY[2] / 16f, (16f - torchZ[2]) / 16f + .03f, -1, -1, false);
            rt.renderPartModel(_chipYellow, "base", (16f - torchX[2]) / 16f + .03f, torchY[2] / 16f, (16f - torchZ[2]) / 16f + .03f, -1, -1, false);
        }
    }

    public static class StateCell extends GateRenderBridge implements Stateless {
        {
            // Wires are {outFront, inBack, inLeft, center, centerright,
            // outRight }
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 8.5f }, { 8.5f, 8.5f, 8.5f }, { 2, 3, 4, 5, 5, 5, 6, 7, 8, 8.5f }, { 8.5f, 8.5f, 8.5f }, { 9.5f, 10.5f, 11.5f, 12.5f, 13, 13, 13 }, { 15, 14, 13 } };
            wirePosZ = new float[][] { { 2, 3, 4, 5 }, { 15, 14, 13 }, { 8.5f, 8.5f, 8.5f, 8.5f, 8f, 7f, 7, 7, 7, 7 }, { 11, 10, 9 }, { 7f, 7f, 7f, 7f, 7f, 8, 8.5f }, { 8.5f, 8.5f, 8.5f } };
            wireColor = new int[] { 0, 0, 0, 0, 0, 0 };
            torchX = new float[] { 13f };
            torchY = new float[] { 0f };
            torchZ = new float[] { 8.5f };
            torchState = new boolean[] { false };
            pointerX = new float[] { 8.5f };
            pointerY = new float[] { 0f };
            pointerZ = new float[] { 12f };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 2) != 0 ? ON : OFF;
            wireColor[2] = (renderState & 4) != 0 ? ON : OFF;
            wireColor[3] = (renderState & 16) == 0 ? ON : OFF;
            wireColor[4] = (renderState & 8) != 0 ? ON : OFF;
            wireColor[5] = (renderState & 64) != 0 ? ON : OFF;
            torchState[0] = (renderState & 8) != 0;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = OFF;
            wireColor[3] = ON;
            wireColor[4] = OFF;
            wireColor[5] = OFF;
            torchState[0] = false;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR) {
                return;
            }
            rt.renderPartModel(_chipRed, "base", (16f - 8.5f) / 16 + .03f, 0, (16f - 7f) / 16 + .03f, -1, -1, false);
            rt.renderPartModel(_chipRed, (wireColor[0] == ON && wireColor[3] == OFF ? "diodeon" : "diodeoff"), (16f - 8.5f) / 16 + .03f, 0, (16f - 7f) / 16 + .03f, -1, -1, false);
        }
    }

    public static class Synchronizer extends GateRenderBridge implements Stateless {
        {
            // Wires are {leftin, rightin, backin, frontout, rightlatch,
            // leftlatch}
            wirePosX = new float[][] { { 2, 3, 4 }, { 15, 14, 13 }, { 8.5f, 8.5f, 8.5f, 8.5f, 7.5f, 6.5f, 6, 6, 6, 9.5f, 10.5f, 11, 11, 11 }, { 8.5f, 8.5f, 8.5f }, { 6, 6, 6, 6, 7, 8, 8.5f }, { 11, 11, 11, 11, 10, 9, 8.5f } };
            wirePosZ = new float[][] { { 8.5f, 8.5f, 8.5f }, { 8.5f, 8.5f, 8.5f }, { 15, 14, 13, 12, 12, 12, 12, 11, 10, 12, 12, 12, 11, 10 }, { 2, 3, 4 }, { 7, 6, 5, 4, 4, 4, 4, }, { 7, 6, 5, 4, 4, 4, 4 } };
            wireColor = new int[] { 0, 0, 0, 0, 0, 0 };
            torchX = new float[] { 8.5f };
            torchY = new float[] { 0f };
            torchZ = new float[] { 4f };
            torchState = new boolean[] { false };
        }

        @Override
        public void set(int renderState) {
            torchState[0] = (renderState & 8) != 0;
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 2) != 0 ? ON : OFF;
            wireColor[2] = (renderState & 4) != 0 ? ON : OFF;
            wireColor[3] = (renderState & 64) != 0 ? ON : OFF;
            wireColor[4] = (renderState & 16) != 0 ? OFF : ON;
            wireColor[5] = (renderState & 32) != 0 ? OFF : ON;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = OFF;
            wireColor[3] = OFF;
            wireColor[4] = ON;
            wireColor[5] = ON;
            torchState[0] = false;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR) {
                return;
            }
            // left latch
            rt.renderPartModel(_chipRed, "base", (16f - 6) / 16 + .03f, 0, (16f - 8.5f) / 16 + .03f, -1, -1, false);
            rt.renderPartModel(_chipRed, (wireColor[4] == ON ? "diodeon" : "diodeoff"), (16f - 6f) / 16 + .03f, 0, (16f - 8.5f) / 16 + .03f, -1, -1, false);
            // right latch
            rt.renderPartModel(_chipRed, "base", (16f - 11) / 16 + .03f, 0, (16f - 8.5f) / 16 + .03f, -1, -1, false);
            rt.renderPartModel(_chipRed, (wireColor[5] == ON ? "diodeon" : "diodeoff"), (16f - 11) / 16 + .03f, 0, (16f - 8.5f) / 16 + .03f, -1, -1, false);
        }
    }

    public static class DLatch extends GateRenderBridge implements Stateless {
        {
            // Wires are {inBack, inRight, outFront, outLeft}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 15, 14, 14, 14, 13, 12, 11, 10, 9, 8.5f }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 2, 3, 4, 5, 5, 5, 6, 7, 8, 8.5f } };
            wirePosZ = new float[][] { { 15, 14, 13, 12, 11 }, { 8.5f, 8.5f, 9.5f, 10f, 10, 10, 10, 10, 10, 10 }, { 2, 3, 4, 5, 6, 7, 8, 9 }, { 8.5f, 8.5f, 8.5f, 8.5f, 9.5f, 10, 10, 10, 10, 10 } };
            wireColor = new int[] { 0, 0, 0, 0 };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 2) != 0 ? ON : OFF;
            wireColor[2] = (renderState & 4) != 0 ? ON : OFF;
            wireColor[3] = (renderState & 8) != 0 ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = OFF;
            wireColor[3] = OFF;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR) {
                return;
            }
            rt.renderPartModel(_chipRed, "base", (16f - 8.5f) / 16 + .03f, 0, (16f - 10) / 16 + .03f, -1, -1, false);
            rt.renderPartModel(_chipRed, (wireColor[2] == ON ? "diodeon" : "diodeoff"), (16f - 8.5f) / 16 + .03f, 0, (16f - 10) / 16 + .03f, -1, -1, false);
        }
    }

    public static class DFlop extends GateRenderBridge implements Stateless {
        {
            // Wires are {backin, rightin, frontOut, leftOut}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 15, 14, 14, 14, 13, 12, 11, 10, 9, 8.5f }, { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f }, { 2, 3, 4, 5, 5, 5, 6, 7, 8, 8.5f } };
            wirePosZ = new float[][] { { 15, 14, 13, 12, 11 }, { 8.5f, 8.5f, 9.5f, 10f, 10, 10, 10, 10, 10, 10 }, { 2, 3, 4, 5, 6, 7, 8, 9 }, { 8.5f, 8.5f, 8.5f, 8.5f, 9.5f, 10, 10, 10, 10, 10 } };
            wireColor = new int[] { 0, 0, 0, 0 };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            wireColor[1] = (renderState & 2) != 0 ? ON : OFF;
            wireColor[2] = (renderState & 4) != 0 ? ON : OFF;
            wireColor[3] = (renderState & 8) != 0 ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
            wireColor[1] = OFF;
            wireColor[2] = OFF;
            wireColor[3] = OFF;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR) {
                return;
            }
            // Chip
            rt.renderPartModel(_chipRed, "base", (16f - 8.5f) / 16 + .03f, 0, (16f - 10) / 16 + .03f, -1, -1, false);
            rt.renderPartModel(_chipRed, (wireColor[3] == ON ? "diodeon" : "diodeoff"), (16f - 8.5f) / 16 + .03f, 0, (16f - 10) / 16 + .03f, -1, -1, false);
            // Diode
            rt.renderPartModel(_diode, "base", (16f - 11.5f) / 16 + .03f, 0, (16f - 10) / 16 + .03f, 270, -1, false);
        }
    }

    public static class BundledLatch extends GateRenderBridge implements Stateless {
        {
            // Wires are {rightIn}
            wirePosX = new float[][] { { 15, 14, 13, 12, 15, 14, 13, 12 } };
            wirePosZ = new float[][] { { 8, 8, 8, 8, 9, 9, 9, 9 } };
            wireColor = new int[] { 0 };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR) {
                return;
            }
            rt.renderPartModel(_latchCableCover, (wireColor[0] == ON ? "on" : "off"), (16f - 8.5f) / 16 + .03f, 0, (16f - 8.5f) / 16 + .03f, -1, -1, false);
            /*WireRenderDummy dummy = new WireRenderDummy();
            dummy.connects[Rotator.relativeToAbsolute(rt.side, rt.front, FRONT)] = true;
            dummy.connects[Rotator.relativeToAbsolute(rt.side, rt.front, BACK)] = true;
            WireRenderAssistant wra = new WireRenderAssistant();
            wra.side = rt.side;
            wra.x = rt.x;
            wra.y = rt.y;
            wra.z = rt.z;
            wra.scaleFactor = 1.0003;
            wra.setWireRenderState(dummy);
            wra.wireIcon = EnumWire.BUNDLED_N.wireSprites[0];
            wra.model = EnumWire.BUNDLED_N.wireMap;
            BasicRenderUtils.setFullColor();
            wra.pushRender();*/
        }
    }

    public static class BundledRelay extends GateRenderBridge implements Stateless {
        {
            // Wires are {rightin}
            wirePosX = new float[][] { { 15, 14, 13, 12, 15, 14, 13, 12 } };
            wirePosZ = new float[][] { { 8, 8, 8, 8, 9, 9, 9, 9 } };
            wireColor = new int[] { 0 };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR) {
                return;
            }
            rt.renderPartModel(_relayCableCover, (wireColor[0] == ON ? "on" : "off"), (16f - 8.5f) / 16 + .03f, 0, (16f - 8.5f) / 16 + .03f, -1, -1, false);
            /*WireRenderDummy dummy = new WireRenderDummy();
            dummy.connects[Rotator.relativeToAbsolute(rt.side, rt.front, FRONT)] = true;
            dummy.connects[Rotator.relativeToAbsolute(rt.side, rt.front, BACK)] = true;
            WireRenderAssistant wra = new WireRenderAssistant();
            wra.x = rt.x;
            wra.y = rt.y;
            wra.z = rt.z;
            wra.side = rt.side;
            wra.scaleFactor = 1.0003;
            wra.setWireRenderState(dummy);
            wra.wireIcon = EnumWire.BUNDLED_N.wireSprites[0];
            wra.model = EnumWire.BUNDLED_N.wireMap;
            BasicRenderUtils.setFullColor();
            wra.pushRender();*/
        }

    }

    public static class BundledMultiplexer extends GateRenderBridge implements Stateless {
        {
            // Wires are {back in}
            wirePosX = new float[][] { { 8, 8, 8, 8, 9, 9, 9, 9 } };
            wirePosZ = new float[][] { { 15, 14, 13, 12, 15, 14, 13, 12 } };
            wireColor = new int[] { 0 };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = OFF;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR) {
                return;
            }
            rt.renderPartModel(_multCableCover, (wireColor[0] == ON ? "on" : "off"), (16f - 8.5f) / 16 + .03f, 0, (16f - 8.5f) / 16 + .03f, -1, -1, false);
            /*WireRenderDummy dummy = new WireRenderDummy();
            dummy.connects[Rotator.relativeToAbsolute(rt.side, rt.front, FRONT)] = true;
            dummy.connects[Rotator.relativeToAbsolute(rt.side, rt.front, LEFT)] = true;
            dummy.connects[Rotator.relativeToAbsolute(rt.side, rt.front, RIGHT)] = true;
            WireRenderAssistant wra = new WireRenderAssistant();
            wra.x = rt.x;
            wra.y = rt.y;
            wra.z = rt.z;
            wra.side = rt.side;
            wra.scaleFactor = 1.0003;
            wra.setWireRenderState(dummy);
            wra.wireIcon = EnumWire.BUNDLED_N.wireSprites[0];
            wra.model = EnumWire.BUNDLED_N.wireMap;
            BasicRenderUtils.setFullColor();
            wra.pushRender();*/

        }
    }

    public static class LightSensor extends GateRenderBridge {
        {
            // Wires are {backout}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, } };
            wirePosZ = new float[][] { { 15, 14, 13, 12, 11, 10 } };
            wireColor = new int[] { 0 };
        }

        // Between 0 (low) and 4 (high), and 5 for dynamic.
        public short solarThreshold = 0;

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
            solarThreshold = (renderState & 2) != 0 ? 0 : solarThreshold;
            solarThreshold = (renderState & 4) != 0 ? 1 : solarThreshold;
            solarThreshold = (renderState & 8) != 0 ? 2 : solarThreshold;
            solarThreshold = (renderState & 16) != 0 ? 3 : solarThreshold;
            solarThreshold = (renderState & 32) != 0 ? 4 : solarThreshold;
            solarThreshold = (renderState & 64) != 0 ? 5 : solarThreshold;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = ON;
            solarThreshold = 1;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR) {
                return;
            }
            rt.renderPartModel(_solar, "cover" + solarThreshold, (16f - 8.5f) / 16 + .03f, 0, (16f - 6.5f) / 16 + .03f, -1, -1, false);
        }
    }

    public static class RainSensor extends GateRenderBridge {
        {
            // Wires are {backout}
            wirePosX = new float[][] { { 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, 8.5f, } };
            wirePosZ = new float[][] { { 15, 14, 13, 12, 11, 10 } };
            wireColor = new int[] { 0 };
        }

        @Override
        public void set(int renderState) {
            wireColor[0] = (renderState & 1) != 0 ? ON : OFF;
        }

        @Override
        public void setItemRender() {
            wireColor[0] = ON;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR) {
                return;
            }
            rt.renderPartModel(_rainSensor, "base", (16f - 8.5f) / 16 + .03f, 0, (16f - 8.5f) / 16 + .03f, -1, -1, false);
            rt.renderPartModel(_rainSensor, "sensor", (16f - 8.5f) / 16 + .03f, 0, (16f - 8.5f) / 16 + .03f, -1, -1, false);
        }
    }

    public static class CCIOExpander extends GateRenderBridge implements Stateless {

        int[] outputs = new int[16];
        boolean isAttached;

        @Override
        public void set(int renderState) {
            outputs = new int[16];
            for (int k = 0; k < 16; k++) {
                outputs[k] = ((renderState & 1) != 0) ? (byte) 255 : 0;
                renderState >>= 1;
            }
            for (int i = 0; i < 16; i++) {
                if (outputs[i] != 0) {
                    isAttached = true;
                    break;
                }
            }
        }

        @Override
        public void setItemRender() {
            outputs = new int[16];
            outputs[3] = 255;
            outputs[7] = 255;
            outputs[9] = 255;
            outputs[12] = 255;
            outputs[13] = 255;
            isAttached = false;
        }

        @Override
        public void renderSpecials(RotatedRenderer rt, boolean isTESR) {
            if (isTESR) {
                return;
            }
            rt.renderPartModel(_serialBus, "base", (16f - 8.5f) / 16 + .03f, 0, (16f - 8.4f) / 16 + .03f, -1, -1, false);
            rt.renderPartModel(_wire, "border", (16f - 14f) / 16 + .03f, 0, (16f - 5.5f) / 16 + .03f, -1, -1, false);
            if (isAttached) {
                rt.renderPartModel(_wire, "wire", (16f - 14f) / 16 + .03f, 0, (16f - 5.5f) / 16 + .03f, -1, PRColors.GREEN.hex, false);
            } else {
                rt.renderPartModel(_wire, "wire", (16f - 14f) / 16 + .03f, 0, (16f - 5.5f) / 16 + .03f, -1, PRColors.RED.hex, false);
            }
            rt.renderPartModel(_lightPanel, "base", (16f - 8.5f) / 16 + .03f, 0, (16f - 8.5f) / 16 + .03f, -1, -1, false);
            for (int i = 0; i < 16; i++) {
                if (outputs[i] != 0) {
                    rt.renderPartModel(_panelLights[i], "base", (16f - 8.5f) / 16 + .03f, 0, (16f - 8.5f) / 16 + .03f, -1, -1, false);
                }
            }
            /*
            WireRenderDummy dummy = new WireRenderDummy();
            dummy.connects[Rotator.relativeToAbsolute(rt.side, rt.front, BACK)] = true;
            WireRenderAssistant wra = new WireRenderAssistant();
            wra.x = rt.x;
            wra.y = rt.y;
            wra.z = rt.z;
            wra.side = rt.side;
            wra.scaleFactor = 1.0003;
            wra.setWireRenderState(dummy);
            wra.wireIcon = EnumWire.BUNDLED_N.wireSprites[0];
            wra.model = EnumWire.BUNDLED_N.wireMap;
            BasicRenderUtils.setFullColor();
            wra.pushRender();
            */
        }
    }
}
