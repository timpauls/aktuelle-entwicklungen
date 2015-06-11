package ueb03;

public class Main {
    public static void main(String[] args) {
        final SyncVar<Integer> syncVar = new SyncVar<Integer>();

        Thread writer = new Thread(new Runnable() {
            @Override
            public void run() {
                int i = 0;
                while(true) {
                    ++i;
                    try {
                        Thread.sleep(10);
                        syncVar.put(i);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        });

        Thread writer2 = new Thread(new Runnable() {
            @Override
            public void run() {
                while(true) {
                    try {
                        Thread.sleep(3);
                        syncVar.put(9001);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        });

        Thread reader = new Thread(new Runnable() {
            @Override
            public void run() {
                while(true) {
                    try {
                        Thread.sleep(5);
                        System.out.println(syncVar.take());
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        });

        Thread reader2 = new Thread(new Runnable() {
            @Override
            public void run() {
                while(true) {
                    try {
                        Thread.sleep(7);
                        System.out.println(syncVar.take());
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
            }
        });

        // Test multiple writers, one reader
        //reader.start();
        //writer.start();
        //writer2.start();

        // Test writer blocking without reader
        //writer.start();
        //try {
        //    Thread.sleep(2000);
        //} catch (InterruptedException e) {
        //    e.printStackTrace();
        //}

        //reader.start();

        // Test multiple reader and one writer.
        reader.start();
        reader2.start();
        writer.start();
    }
}
