package ueb01;

import java.util.ArrayList;
import java.util.List;

public class Main {
    public static List<Stick> sticks;
    public static List<Philosopher> philosophers;

    private static void init(int count) {
        sticks = new ArrayList<Stick>();
        philosophers = new ArrayList<Philosopher>();
        for (int i = 0; i < count; i++) {
            sticks.add(new Stick(i));
        }

        for (int i = 0; i < count; i++) {
            philosophers.add(new Philosopher(sticks.get(i), sticks.get((i+1) % count), i));
        }
    }

    public static void run() {
        for (Philosopher philosopher : philosophers) {
            new Thread(philosopher).start();
        }
    }
    public static void main(String[] args) {
        init(2);
        run();
    }
}
