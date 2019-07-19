public class Sampler implements UGen {
    private int initialX = 0;
    private double x = 0;
    public boolean ended = false;
    private CubicSplineFast cs;
    private double lastX, gain;

    public Sampler(int initialX, CubicSplineFast cs, double gain, int offset) {
        this.initialX = initialX;
        this.cs = cs;
        this.x = offset;
        this.gain = gain;
        lastX = cs.lastX();
    }


    public float[] process(int count, float[][] inputs) {
        float[] out = new float[count];
        float[] rate = inputs[0];

        for (int i = - this.initialX; i<count; i++) {
            out[i] = (float)(gain * cs.interpolate(x));
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
