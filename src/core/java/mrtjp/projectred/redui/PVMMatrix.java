package mrtjp.projectred.redui;

import codechicken.lib.vec.Matrix4;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.matrix.MatrixStack;

/**
 * Calculates a view matrix based on camera position, x rotation, and y rotation.
 */
public class PVMMatrix {

    private final Matrix4 projectionMatrix = new Matrix4();
    private final Matrix4 viewMatrix = new Matrix4();
    private final Matrix4 orientationMatrix = new Matrix4();

    private final Vector3 forward = new Vector3();
    private final Vector3 right = new Vector3();
    private final Vector3 up = new Vector3();

    private final Vector3 cameraPosition = new Vector3(0, 0, 0);

    private double fovY;
    private double width;
    private double height;
    private double zNear;
    private double zFar;

    public void setProjection(double fovY, double windowWidth, double windowHeight, double zNear, double zFar) {
        this.fovY = fovY;
        this.width = windowWidth;
        this.height = windowHeight;
        this.zNear = zNear;
        this.zFar = zFar;
        projectionMatrix.setIdentity();
        perspective(projectionMatrix, fovY, windowWidth / windowHeight, zNear, zFar);
    }

    public void setView(double x, double y, double z, double xRot, double yRot) {
        cameraPosition.set(x, y, z);

        orientationMatrix.setIdentity();
        orientationMatrix.rotate(-yRot, Vector3.Y_POS);
        orientationMatrix.rotate(-xRot, Vector3.X_POS);

        forward.set(Vector3.Z_NEG);
        right.set(Vector3.X_POS);
        up.set(Vector3.Y_POS);

        orientationMatrix.apply(forward);
        orientationMatrix.apply(right);
        orientationMatrix.apply(up);

        viewMatrix.setIdentity();
        lookAt(viewMatrix, cameraPosition, cameraPosition.copy().add(forward), up);
        viewMatrix.transpose();
    }

    public Matrix4 getProjectionMatrix() {
        return projectionMatrix;
    }

    public Matrix4 getViewMatrix() {
        return viewMatrix;
    }

    public MatrixStack getModelViewMatrixStack() {
        MatrixStack stack = new MatrixStack();
        stack.pushPose();
        stack.last().pose().set(viewMatrix.toMatrix4f());
        return stack;
    }

    /**
     * Projects NDC coordinates [-1, 1] onto a target plane at some fixed distance
     * to the camera taking into account the FOV. This target plane is assumed to be
     * parallel to the near and far planes of the perspective view frustum.
     *
     * @param ndcX X coordinate on the viewport in [-1, 1] range
     * @param ndcY Y coordinate on the viewport in [-1, 1] range
     * @param planeDist Distance of the projection
     * @return Projection of (ndcX, ndcY) into world-space
     */
    public Vector3 ndcToWorldCoordinates(double ndcX, double ndcY, double planeDist) {
        // Assume camera at 0,0,0 facing -z
        // Find top and right sides of target plane visible in this viewport
        double maxY = Math.tan(fovY / 2) * planeDist;
        double maxX = maxY * width / height;

        // Map ndc points onto plane to get look vector
        double dx = ndcX * maxX;
        double dy = ndcY * maxY;
        Vector3 lookVec = new Vector3(dx, dy, -planeDist);

        // Apply transforms to account for camera position and orientation
        orientationMatrix.apply(lookVec);
        lookVec.add(cameraPosition);

        return lookVec;
    }

    public static void perspective(Matrix4 m, double fovY, double aspect, double zNear, double zFar) {
        double tanHalfFovY = Math.tan(fovY / 2D);
        double deltaZ = zFar - zNear;

        m.m00 = 1D / (aspect * tanHalfFovY);
        m.m11 = 1D / tanHalfFovY;
        m.m22 = -(zFar + zNear) / deltaZ;
        m.m32 = -1;
        m.m23 = -2D * (zFar * zNear) / deltaZ;
        m.m33 = 0;
    }

    public static void orthographic(Matrix4 m, double width, double height, double zNear, double zFar) {
        double deltaZ = zFar - zNear;

        m.m00 = 2 / width;
        m.m11 = 2 / height;
        m.m22 = -2 / deltaZ;
        m.m33 = 1;
        m.m03 = -1;
        m.m13 = 1;
        m.m23 = -(zFar + zNear) / deltaZ;
    }

    public static void lookAt(Matrix4 m, Vector3 eye, Vector3 center, Vector3 up) {
        Vector3 f = center.copy().subtract(eye).normalize();
        Vector3 s = f.copy().crossProduct(up).normalize();
        Vector3 u = s.copy().crossProduct(f);

        m.m00 = s.x;
        m.m10 = s.y;
        m.m20 = s.z;

        m.m01 = u.x;
        m.m11 = u.y;
        m.m21 = u.z;

        m.m02 = -f.x;
        m.m12 = -f.y;
        m.m22 = -f.z;

        m.m30 = -s.dotProduct(eye);
        m.m31 = -u.dotProduct(eye);
        m.m32 = f.dotProduct(eye);
    }
}
