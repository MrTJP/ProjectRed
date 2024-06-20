package mrtjp.projectred.expansion.client;

import codechicken.lib.render.CCModel;
import codechicken.lib.render.lighting.LightModel;
import codechicken.lib.render.model.OBJParser;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import net.minecraft.resources.ResourceLocation;

import javax.annotation.Nullable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

public class TubeModelBuilder {

    private static final Map<String, CCModel> TUBE_MODELS = loadModels("tube", (k, v) -> v.apply(new Translation(Vector3.CENTER)));

    private int modelKey = 0;
    private boolean isWire = false;
    private boolean modelBuilt = false;
    private @Nullable CCModel model = null;

    public TubeModelBuilder setModelKey(int key) {
        this.modelKey = key;
        return this;
    }

    public TubeModelBuilder setWireMode(boolean isWire) {
        this.isWire = isWire;
        return this;
    }

    public CCModel build() {
        if (!modelBuilt) {
            buildModel();
            modelBuilt = true;
            assert model != null;
            return model;
        }

        assert model != null;
        return model.copy();
    }

    private String axisName(int i) {
        return (isWire ? "wire_a" : "a") + i;
    }

    private String centerName() {
        return isWire ? "wire_center" : "center";
    }

    private String sideName(int s) {
        return (isWire ? "wire_s" : "s") + s;
    }

    private void buildModel() {
        int connMap = modelKey & 0x3F;
        // Number of sided connections
        int connCount = countConnections(modelKey);
        // Number of connection axis used
        int axisCount = countAxis(modelKey);

        // Axial model where the 2 conns are opposite. We can use the
        // simplified model for this case.
        if (connCount == 2 && axisCount == 1) {
            int axis = (connMap & 0x3) != 0 ? 0 : (connMap & 0xC) != 0 ? 1 : 2;
            String mKey = axisName(axis);
            model = TUBE_MODELS.get(mKey).copy();
            return;
        }

        // Otherwise, we need to aggregate center model plus all sides
        List<CCModel> modelList = new LinkedList<>();
        modelList.add(TUBE_MODELS.get(centerName())); // No need to copy, they will be copied in combine call
        for (int s = 0; s < 6; s++) {
            if ((connMap & (1 << s)) != 0) {
                String mKey = sideName(s);
                modelList.add(TUBE_MODELS.get(mKey));
            }
        }
        model = CCModel.combine(modelList);
    }


    //region Utils
    public static int countConnections(int connMask) {
        int count = 0;
        for (int s = 0; s < 6; s++) {
            if ((connMask & (1 << s)) != 0) {
                count++;
            }
        }
        return count;
    }

    public static int countAxis(int connMask) {
        int count = 0;
        for (int a = 0; a < 3; a++) {
            if ((connMask & (0x3 << (a * 2))) != 0) {
                count++;
            }
        }
        return count;
    }


    public static Map<String, CCModel> loadModels(String path) {
        return loadModels(path, (k, v) -> { });
    }

    public static Map<String, CCModel> loadModels(String path, BiConsumer<String, CCModel> operation) {
        Map<String, CCModel> models = new OBJParser(new ResourceLocation(MOD_ID, "obj/" + path + ".obj"))
                .ignoreMtl()
                .quads()
                .parse();
        models.replaceAll((k, v) -> v.backfacedCopy());

        for (Map.Entry<String, CCModel> m : models.entrySet()) {
            operation.accept(m.getKey(), m.getValue());
            m.getValue().computeNormals();
            m.getValue().shrinkUVs(0.0005);
            m.getValue().computeLighting(LightModel.standardLightModel);
        }

        return models;
    }
    //endregion
}
