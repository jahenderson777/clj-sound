public class Player extends CubicSplineFast implements UGen {
    private int initialX = 0;
    private double x1 = 0;
    public boolean ended = false;

    public Player(int initialX, double[] x, double[] y) {
        super(x,y);
        this.initialX = initialX;
    }

    public float[] process(int count, float[][] inputs) {
        float[] out = new float[count];
        if (inputs.length == 0) {
            for (int i = - this.initialX; i<count && this.x1 < this.lastX(); i++) {
                out[i] = (float)this.interpolate((double)this.x1);
                this.x1 += 1.0D;
            }
        } else {
            float[] rate = inputs[0];
            for (int i = - this.initialX; i<count && this.x1 < this.lastX(); i++) {
                out[i] = (float)this.interpolate((double)this.x1);
                this.x1 += rate[i];
            }
        }
        this.initialX = 0;
        if (this.x1 >= this.lastX())
            this.ended = true;
        return out;
    }

}
