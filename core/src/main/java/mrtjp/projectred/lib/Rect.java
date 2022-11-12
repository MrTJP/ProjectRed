package mrtjp.projectred.lib;

public class Rect {

    public static final Rect ZERO = new Rect(Point.ZERO, Size.ZERO);
    public static final Rect INFINITE = new Rect(new Point(Integer.MIN_VALUE / 2, Integer.MIN_VALUE / 2), Size.INFINITE);

    public final Point origin;
    public final Size size;

    public Rect(Point origin, Size size) {
        this.origin = origin;
        this.size = size;
    }

    public Rect(int x, int y, int width, int height) {
        this(new Point(x, y), new Size(width, height));
    }

    public Rect(Point min, Point max) {
        this(min, new Size(max.x - min.x, max.y - min.y));
    }

    public int x() {
        return origin.x;
    }

    public int y() {
        return origin.y;
    }

    public int width() {
        return size.width;
    }

    public int height() {
        return size.height;
    }

    public int maxX() {
        return origin.x + size.width;
    }

    public int maxY() {
        return origin.y + size.height;
    }

    public Point maxPoint() {
        return new Point(maxX(), maxY());
    }

    public int midX() {
        return origin.x + size.width / 2;
    }

    public int midY() {
        return origin.y + size.height / 2;
    }

    public Point midPoint() {
        return new Point(midX(), midY());
    }

    public boolean contains(Point point) {
        return point.x >= origin.x && point.y >= origin.y && point.x <= maxX() && point.y <= maxY();
    }

    public boolean contains(Rect rect) {
        return contains(rect.origin) && contains(rect.maxPoint());
    }

    public boolean intersects(Rect rect) {
        return contains(rect.origin) || contains(rect.maxPoint()) || rect.contains(origin) || rect.contains(maxPoint());
    }

    public Rect enclose(Point p) {
        int x = Math.min(origin.x, p.x);
        int y = Math.min(origin.y, p.y);
        int w = Math.max(maxX(), p.x) - x;
        int h = Math.max(maxY(), p.y) - y;
        return new Rect(x, y, w, h);
    }

    public Rect union(Rect rect) {
        if (this.equals(ZERO)) return rect;
        if (rect.equals(ZERO)) return this;
        int x = Math.min(origin.x, rect.x());
        int y = Math.min(origin.y, rect.y());
        int w = Math.max(maxX(), rect.maxX()) - x;
        int h = Math.max(maxY(), rect.maxY()) - y;
        return new Rect(x, y, w, h);
    }

    public Rect expand(int x, int y) {
        int dw = width() + x < 0 ? -width() : x;
        int dh = height() + y < 0 ? -height() : y;
        return new Rect(origin.subtract(dw / 2, dh / 2), new Size(width() + dw, height() + dh));
    }

    public Rect trap(Rect rect) {
        int dx = (rect.x() < x() ? x() - rect.x() : 0) + (rect.maxX() > maxX() ? rect.maxX() - maxX() : 0);
        int dy = (rect.y() < y() ? y() - rect.y() : 0) + (rect.maxY() > maxY() ? rect.maxY() - maxY() : 0);
        return new Rect(rect.x() + dx, rect.y() + dy, rect.width(), rect.height());
    }

    public Vec2 ndc(Point p) {
        double dx = (2D * (p.x - origin.x) / width()) - 1;
        double dy = (2D * (p.y - origin.y) / height()) - 1;
        return new Vec2(dx, -dy);
    }
}
