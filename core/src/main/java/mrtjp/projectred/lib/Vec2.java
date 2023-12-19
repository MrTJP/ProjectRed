package mrtjp.projectred.lib;

public class Vec2 {

    public static final Vec2 ZERO = new Vec2(0, 0);

    public final double dx;
    public final double dy;

    public Vec2(double dx, double dy) {
        this.dx = dx;
        this.dy = dy;
    }

    public Vec2 add(Vec2 other) {
        return new Vec2(dx + other.dx, dy + other.dy);
    }

    public Vec2 subtract(Vec2 other) {
        return new Vec2(dx - other.dx, dy - other.dy);
    }

    public Vec2 multiply(Vec2 other) {
        return new Vec2(dx * other.dx, dy * other.dy);
    }

    public Vec2 divide(Vec2 other) {
        return new Vec2(dx / other.dx, dy / other.dy);
    }

    public Vec2 add(double dx, double dy) {
        return new Vec2(this.dx + dx, this.dy + dy);
    }

    public Vec2 subtract(double dx, double dy) {
        return new Vec2(this.dx - dx, this.dy - dy);
    }

    public Vec2 multiply(double dx, double dy) {
        return new Vec2(this.dx * dx, this.dy * dy);
    }

    public Vec2 divide(double dx, double dy) {
        return new Vec2(this.dx / dx, this.dy / dy);
    }

    public Vec2 add(double d) {
        return add(d, d);
    }

    public Vec2 subtract(double d) {
        return subtract(d, d);
    }

    public Vec2 multiply(double d) {
        return multiply(d, d);
    }

    public Vec2 divide(double d) {
        return divide(d, d);
    }

    public double dot(Vec2 other) {
        return dx * other.dx + dy * other.dy;
    }

    public double dot(double dx, double dy) {
        return this.dx * dx + this.dy * dy;
    }

    public double mag() {
        return Math.sqrt(dx * dx + dy * dy);
    }

    public double magSquared() {
        return dx * dx + dy * dy;
    }

    public Vec2 normalize() {
        double mag = mag();
        return mag == 0 ? ZERO : new Vec2(dx / mag, dy / mag);
    }

    public Vec2 negate() {
        return new Vec2(-dx, -dy);
    }

    public Vec2 flip() {
        return new Vec2(dy, dx);
    }

    public Vec2 project(Vec2 other) {
        double d = dot(other) / other.magSquared();
        return other.multiply(d, d);
    }

    public Vec2 reject(Vec2 other) {
        return subtract(project(other));
    }

    public Vec2 axialProject() {
        if (Math.abs(dx) > Math.abs(dy)) {
            return new Vec2(dx, 0);
        } else {
            return new Vec2(0, dy);
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Vec2 other) {
            return dx == other.dx && dy == other.dy;
        }
        return false;
    }

    @Override
    public String toString() {
        return "Vec2[" + dx + ", " + dy + "]";
    }
}
