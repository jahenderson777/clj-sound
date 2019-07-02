public class Player extends CubicSplineFast implements UGen {
    private int initialX = 0;
    private double lastX = 0;

    public Player(int initialX, double[] x, double[] y) {
        super(x,y);
        this.initialX = initialX;
    }

    public float[] process(int count, float[][] inputs) {
        float[] out = new float[count];
        if (inputs.length == 0) {
            for (int i = - this.initialX; i<count; i++) {
                out[i] = (float)this.interpolate((double)this.lastX);
                this.lastX += 1.0D;
            }
        } else {
            float[] rate = inputs[0];
            for (int i = - this.initialX; i<count; i++) {
                out[i] = (float)this.interpolate((double)this.lastX);
                this.lastX += rate[i];
            }
        }
        this.initialX = 0;
        return out;
    }
}
