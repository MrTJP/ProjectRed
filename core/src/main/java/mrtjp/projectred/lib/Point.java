package mrtjp.projectred.lib;

public class Point {

    public static final Point ZERO = new Point(0, 0);
    public static final Point INFINITE = new Point(Integer.MAX_VALUE, Integer.MAX_VALUE);

    public static final Point[] DIR_OFFSETS = { new Point(0, -1), new Point(1, 0), new Point(0, 1), new Point(-1, 0) };

    public final int x;
    public final int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public Point add(Point p) {
        return new Point(x + p.x, y + p.y);
    }

    public Point subtract(Point p) {
        return new Point(x - p.x, y - p.y);
    }

    public Point multiply(Point p) {
        return new Point(x * p.x, y * p.y);
    }

    public Point divide(Point p) {
        return new Point(x / p.x, y / p.y);
    }

    public Point add(int x, int y) {
        return new Point(this.x + x, this.y + y);
    }

    public Point subtract(int x, int y) {
        return new Point(this.x - x, this.y - y);
    }

    public Point multiply(int x, int y) {
        return new Point(this.x * x, this.y * y);
    }

    public Point divide(int x, int y) {
        return new Point(this.x / x, this.y / y);
    }

    public Point negate() {
        return new Point(-x, -y);
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public Point flip() {
        return new Point(y, x);
    }

    public Vec2 toVec2() {
        return new Vec2(x, y);
    }

    public Point max(Point p) {
        return new Point(Math.max(x, p.x), Math.max(y, p.y));
    }

    public Point min(Point p) {
        return new Point(Math.min(x, p.x), Math.min(y, p.y));
    }

    public Point max(int x, int y) {
        return new Point(Math.max(this.x, x), Math.max(this.y, y));
    }

    public Point min(int x, int y) {
        return new Point(Math.min(this.x, x), Math.min(this.y, y));
    }

    public Point clamp(Rect rect) {
        return this.min(rect.maxPoint()).max(rect.origin);
    }

    public Point clamp(Size size) {
        return this.min(size.width, size.height).max(ZERO);
    }

    public Point offset(int r) {
        return offset(r, 1);
    }

    public Point offset(int r, int amount) {
        return new Point(x + DIR_OFFSETS[r].x * amount, y + DIR_OFFSETS[r].y * amount);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Point) {
            Point p = (Point) obj;
            return x == p.x && y == p.y;
        }
        return false;
    }

    @Override
    public String toString() {
        return "Point[" + x + ", " + y + "]";
    }
}
