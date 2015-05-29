package ueb04;

import java.util.LinkedList;
import java.util.List;

/**
 * Created by tim on 28.05.15.
 */
public class ChannelReader implements Runnable {
    private Chan<Long> chan;
    private List<Long> received;

    public ChannelReader(Chan<Long> chan) {
        this.chan = chan;
        received = new LinkedList<Long>();
    }

    @Override
    public void run() {
        while (true) {
            try {
                Long read = chan.read();
                received.add(read);
                System.out.println(read);
                checkInvariant();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    private void checkInvariant() {
        for (int i = 0; i < received.size(); ++i) {
            assert (received.get(i).equals(i * (i + 1) / 2l));
        }
    }
}
