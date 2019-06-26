public class SawTooth {
    private int initialX = 0;
    private float y = -1;

    public SawTooth(int initialX) {
        this.initialX = initialX;
    }

    public void process(int count, float[] buf, float[] freq) {
        float y1 = this.y;
        for (int i = this.initialX; i<count; i++) {
            buf[i] = y1 - 1.0f / freq[i];
            if (buf[i] <= -1.0f)
                buf[i] = 1.0f;
            y1 = buf[i];
        }
    }
}
