public class Dist implements UGen {
    private int initialX = 0;
    private CubicSplineFast shape;

    public Dist(int initialX) {
        double[] x = {0.0, 0.6, 0.9, 1.0};
        double[] y = {0.0, 1.2, 0.6, 0.6};
        this.initialX = initialX;
        shape = new CubicSplineFast(x, y);
    }

    public float[] process(int count, float[][] inputs) {
        float[] out = new float[count];
        float[] input = inputs[0];
        float[] gain = inputs[1];
        float y;
        for (int i = - initialX; i<count; i++) {
            y = input[i];
            if (y>1.0f) {
                out[i] = 0.6f;
            } else if (y<-1.0f) {
                out[i] = -0.6f;
            } else if (y>0.0f) {
                out[i] = (float)shape.interpolate(y * gain[i]);
            } else {
                out[i] = (float)-shape.interpolate(-y * gain[i]);
            }
        }
        initialX = 0;
        return out;
    }
}
