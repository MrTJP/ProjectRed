package mrtjp.projectred.core;

import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.uv.UV;
import codechicken.lib.vec.uv.UVTransformation;

public class UVT extends UVTransformation {

    private final Transformation t;
    private final Vector3 vec = new Vector3();

    public UVT(Transformation t) {
        this.t = t;
    }

    @Override
    public void apply(UV uv) {
        vec.set(uv.u, 0, uv.v).apply(t);
        uv.set(vec.x, vec.z);
    }

    @Override
    public UVTransformation inverse() {
        return new UVT(t.inverse());
    }

    @Override
    public UVTransformation copy() {
        return new UVT(t.copy());
    }
}
