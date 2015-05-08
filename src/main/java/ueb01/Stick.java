package ueb01;

public class Stick {
    private boolean taken = false;
    public int id;

    public Stick(int id) {
        this.id = id;
    }

    public synchronized void take() {
        while (taken) {
            try {
                this.wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        taken = true;
        System.out.println(String.format("ueb01.Stick %d taken", id));
        System.out.flush();
    }

    public synchronized void put() {
        taken = false;
        System.out.println(String.format("ueb01.Stick %d released.", id));
        System.out.flush();
        this.notifyAll();
    }

    public synchronized boolean isTaken() {
        return taken;
    }
}
