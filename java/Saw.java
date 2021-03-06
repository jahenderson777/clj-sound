public class Saw implements UGen{
    private int initialX = 0;
    private float y = -1;

    public Saw(int initialX) {
        this.initialX = initialX;
    }

    public float[] process(int count, float[][] inputs) {
        float[] out = new float[count];
        float[] freq = inputs[0];
        float y1 = this.y;
        for (int i = - this.initialX; i<count; i++) {
            out[i] = y1 - (freq[i] / 10000.0f);
            if (out[i] <= -1.0f)
                out[i] += 1.0f;
            y1 = out[i];
        }
        this.y = y1;
        this.initialX = 0;
        return out;
    }
}
