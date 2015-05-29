package ueb04;

/**
 * Created by tim on 28.05.15.
 */
public class ChannelWriter implements Runnable {
    private Chan<Long> chan;

    public ChannelWriter(Chan<Long> chan) {
        this.chan = chan;
    }

    @Override
    public void run() {
        for (long i = 0; i < Long.MAX_VALUE; ++i) {
            try {
                chan.write(i * (i + 1) / 2);
                Thread.sleep(100);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
}
