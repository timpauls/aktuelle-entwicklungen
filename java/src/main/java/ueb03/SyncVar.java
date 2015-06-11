package ueb03;


public class SyncVar<T> {
    private T content;

    private int readerCount;
    private int writerCount;

    private final Object reader = new Object();
    private final Object writer = new Object();

    public SyncVar() {
        this.content = null;
        this.readerCount = 0;
        this.writerCount = 0;
    }

    public void put(T o) throws InterruptedException {
        synchronized (writer) {
            ++writerCount;
            while (readerCount == 0) {
                writer.wait();
            }
            --readerCount;

            synchronized (reader) {
                content = o;
                reader.notify();
            }
        }
    }

    public T take() throws InterruptedException {
        synchronized (reader) {
            ++readerCount;

            synchronized (writer) {
                writer.notify();
            }

            while (writerCount == 0 || content == null) {
                reader.wait();
            }
            --writerCount;

            T result = content;
            content = null;
            return result;
        }
    }

}
