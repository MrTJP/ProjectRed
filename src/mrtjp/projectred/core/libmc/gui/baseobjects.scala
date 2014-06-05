package mrtjp.projectred.core.libmc.gui

class Point(var x:Int, var y:Int)
{
    def set(x:Int, y:Int) = {this.x = x;this.y = y;this}

    def equals(that:Point) = x == that.x && y == that.y

    def copy = new Point(x, y)

    def negate = new Point(-x, -y)
    def invert = new Point(y, x)

    def add(that:Point) = new Point(x+that.x, y+that.y)
    def subtract(that:Point) = add(-that)
    def multiply(that:Point) = new Point(x*that.x, y*that.y)

    def unary_- = negate
    def unary_~ = invert

    def +(that:Point) = add(that)
    def -(that:Point) = subtract(that)
    def *(that:Point) = multiply(that)

    def >(that:Point) = x>that.x && y>that.y
    def <(that:Point) = x<that.x && y<that.y
    def >=(that:Point) = this>that || this == that
    def <=(that:Point) = this<that || this == that
}

class Rect(var min:Point, var max:Point)
{
    def this(xmin:Int, ymin:Int, xmax:Int, ymax:Int) = this(new Point(xmin, ymin), new Point(xmax, ymax))
    def this(r:Rect) = this(r.min.copy, r.max.copy)
    def this() = this(0, 0, 0, 0)

    def set(xmin:Int, ymin:Int, xmax:Int, ymax:Int):this.type = setMin(xmin, ymin).setMax(xmax, ymax)
    def setMin(x:Int, y:Int):this.type = {min.set(x, y); this}
    def setMax(x:Int, y:Int):this.type = {max.set(x, y); this}

    def set(min:Point, max:Point):this.type = set(min.x, min.y, max.x, max.y)
    def setMin(p:Point):this.type = setMin(p.x, p.y)
    def setMax(p:Point):this.type = setMax(p.x, p.y)

    def width = max.x-min.x
    def height = max.y-min.y

    def setWH(w:Int, h:Int) = setWidth(w).setHeight(h)
    def setWidth(w:Int) = {max.set(min.x+w, max.y); this}
    def setHeight(h:Int) = {max.set(max.x, min.y+h); this}

    def copy = new Rect(min.copy, max.copy)

    def intersects(p:Point) = p >= min && p <= max
    def intersects(r:Rect):Boolean = intersects(r.min) || intersects(r.max) || r.intersects(min) || r.intersects(max)

    def enclose(p:Point) =
    {
        if (min.x > p.x) min.x = p.x
        if (min.y > p.y) min.y = p.y
        if (max.x < p.x) max.x = p.x
        if (max.y < p.y) max.y = p.y
        this
    }
    def enclose(r:Rect) =
    {
        if (min.x > r.min.x) min.x = r.min.x
        if (min.y > r.min.y) min.y = r.min.y
        if (max.x < r.max.x) max.x = r.max.x
        if (max.y < r.max.y) max.y = r.max.y
        this
    }
}