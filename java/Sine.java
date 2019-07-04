public class Sine implements UGen {
    private static float[] genWaveTable() {
        float[] waveTable = new float[65535];
        float x = (float)(2.0D * Math.PI / 65535.0D);
        //float pi4 = (float)(Math.PI / 4.0D);
        for(int i = 0; i<65535; i++) {
            waveTable[i] = (float)java.lang.Math.sin(x * (float)i);
        }
        return waveTable;

    }
    private static float[] waveTable = Sine.genWaveTable();
    private int initialX = 0;
    private double x = 0;
    public boolean ended = false;

    public Sine(int initialX) {
        this.initialX = initialX;
    }

    public float[] process(int count, float[][] inputs) {
        float[] out = new float[count];
        float[] rate = inputs[0];

        for (int i = - this.initialX; i<count; i++) {
            out[i] = Sine.waveTable[(int)this.x];
            this.x += rate[i];
            if ((int)this.x >= 65535)
                this.x -= 65535;
        }

        this.initialX = 0;
        return out;
    }
}
