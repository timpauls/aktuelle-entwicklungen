package ueb04;

/**
 * Created by tim on 28.05.15.
 */
public class MainLoop {
    public static void main(String[] args) {
        Chan<Long> chan = new Chan<Long>();

        new Thread(new ChannelReader(chan)).start();
        new Thread(new ChannelWriter(chan)).start();
    }
}