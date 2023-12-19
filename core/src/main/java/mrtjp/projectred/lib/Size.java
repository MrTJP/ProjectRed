package mrtjp.projectred.lib;

public class Size {

    public static final Size ZERO = new Size(0, 0);
    public static final Size INFINITE = new Size(Integer.MAX_VALUE, Integer.MAX_VALUE);

    public final int width;
    public final int height;

    public Size(int width, int height) {
        this.width = width;
        this.height = height;
    }

    public Size add(int width, int height) {
        return new Size(this.width + width, this.height + height);
    }

    public Size subtract(int width, int height) {
        return new Size(this.width - width, this.height - height);
    }

    public Size multiply(int width, int height) {
        return new Size(this.width * width, this.height * height);
    }

    public Size divide(int width, int height) {
        return new Size(this.width / width, this.height / height);
    }

    public Size add(Size size) {
        return add(size.width, size.height);
    }

    public Size subtract(Size size) {
        return subtract(size.width, size.height);
    }

    public Size multiply(Size size) {
        return multiply(size.width, size.height);
    }

    public Size divide(Size size) {
        return divide(size.width, size.height);
    }

    public Vec2 toVec2() {
        return new Vec2(width, height);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Size size) {
            return size.width == width && size.height == height;
        }
        return false;
    }

    @Override
    public String toString() {
        return "Size[" + width + ", " + height + "]";
    }
}
