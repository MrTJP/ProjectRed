package mrtjp.projectred.redui;

import mrtjp.core.vec.Point;
import mrtjp.core.vec.Rect;
import mrtjp.core.vec.Size;

import javax.annotation.Nullable;
import java.util.LinkedList;
import java.util.List;

public abstract class AbstractGuiNode implements RedUINode {

    private RedUIRootNode root;
    private RedUINode parent;

    private Rect frame = new Rect(Point.zeroPoint(), Size.zeroSize());
    private double zPos = 0;

    private boolean isHidden = false;

    private final List<RedUINode> children = new LinkedList<>();

    public void setPosition(int x, int y) {
        frame = new Rect(new Point(x, y), frame.size());
    }

    public void setSize(int width, int height) {
        frame = new Rect(frame.origin(), new Size(width, height));
    }

    public void setHidden(boolean isHidden) {
        this.isHidden = isHidden;
    }

    @Override
    public RedUIRootNode getRoot() {
        return root;
    }

    @Override
    public void setRoot(@Nullable RedUIRootNode root) {
        this.root = root;
    }

    @Override
    public RedUINode getParent() {
        return parent;
    }

    @Override
    public void setParent(@Nullable RedUINode parent) {
        this.parent = parent;
    }

    @Override
    public List<RedUINode> getOurChildren() {
        return children;
    }

    @Override
    public Rect getFrame() {
        return frame;
    }

    @Override
    public Point getPosition() {
        return frame.origin();
    }

    @Override
    public double getZPosition() {
        return zPos;
    }

    @Override
    public double getRelativeZPosition() {
        return zPos - getParent().getZPosition();
    }

    @Override
    public void setZPosition(double zPosition) {
        this.zPos = zPosition;
    }

    @Override
    public boolean isHidden() {
        return isHidden;
    }
}
