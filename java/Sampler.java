public class Sampler implements UGen {
    private int initialX = 0;
    private double x = 0;
    public boolean ended = false;
    private CubicSplineFast cs;
    private double lastX;

    public Sampler(int initialX, CubicSplineFast cs) {
        this.initialX = initialX;
        this.cs = cs;
        lastX = cs.lastX();
    }

    public float[] process(int count, float[][] inputs) {
        float[] out = new float[count];
        float[] rate = inputs[0];

        for (int i = - this.initialX; i<count; i++) {
            out[i] = (float)cs.interpolate(x);
            x += rate[i];
            if (x >= lastX) {
                ended = true;
                break;
            }
        }

        this.initialX = 0;
        return out;
    }
}
