public class EnvPlayer implements UGen {
    private SplineInterpolator interpolator;
    private int initialX = 0;
    private double x1 = 0;
    private double lastX;
    public boolean ended = false;

    public EnvPlayer(int initialX, double[] x, double[] y) {
        this.interpolator = SplineInterpolator.createMonotoneCubicSpline(x, y);
        this.initialX = initialX;
        this.lastX = x[x.length - 1];
    }

    public float[] process(int count, float[][] inputs) {
        float[] out = new float[count];
        if (inputs.length == 0) {
            for (int i = - this.initialX; i<count && this.x1 < this.lastX; i++) {
                out[i] = (float)interpolator.interpolate(this.x1);
                this.x1 += 1.0D;
            }
        } else {
            float[] rate = inputs[0];
            for (int i = - this.initialX; i<count && this.x1 < this.lastX; i++) {
                out[i] = (float)interpolator.interpolate(this.x1);
                this.x1 += rate[i];
            }
        }
        this.initialX = 0;
        if (this.x1 >= this.lastX)
            this.ended = true;
        return out;
    }
}
