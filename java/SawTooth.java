public class SawTooth extends UGen{
    private int initialX = 0;
    private float y = -1;

    public SawTooth(int initialX) {
        this.initialX = initialX;
    }

    public float[] process(int count, float[][] inputs) {
        float[] out = new float[count];
        float[] freq = inputs[0];
        float y1 = this.y;
        for (int i = this.initialX; i<count; i++) {
            out[i] = y1 - 1.0f / freq[i];
            if (out[i] <= -1.0f)
                out[i] = 1.0f;
            y1 = out[i];
        }
        return out;
    }
}
