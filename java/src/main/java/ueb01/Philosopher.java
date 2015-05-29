package ueb01;

public class Philosopher implements Runnable {
    private static final int EAT_TIME = 3000;
    private static final int THINK_TIME = 1500;
    private final Stick left;
    private final Stick right;
    private final int id;

    public Philosopher(Stick left, Stick right, int id) {
        this.left = left;
        this.right = right;
        this.id = id;
    }

    private void eat() {
        try {
            System.out.println(String.format("ueb01.Philosopher %d is eating.", id));
            Thread.sleep(EAT_TIME);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private void think() {
        try {
            System.out.println(String.format("ueb01.Philosopher %d is eating.", id));
            Thread.sleep(THINK_TIME);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void run() {
        System.out.println(String.format("ueb01.Philosopher %d is alive.", id));
        System.out.flush();
        while(true) {
            System.out.println(String.format("ueb01.Philosopher %d is thinking.", id));
            System.out.flush();
            think();

            left.take();
            if (!right.isTaken()) {
                right.take();
                System.out.println(String.format("ueb01.Philosopher %d is eating.", id));
                System.out.flush();
                eat();
                right.put();
            }
            left.put();
        }
    }
}
