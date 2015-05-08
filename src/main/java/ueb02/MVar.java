package ueb02;


public class MVar<T> {
    private T content;
    private boolean empty;

    private final Object reader = new Object();
    private final Object writer = new Object();

    public MVar(T t) {
        this.content = t;
        this.empty = false;
    }

    public MVar() {
        this.content = null;
        this.empty = true;
    }

    private boolean isEmpty() {
        return empty;
    }

    public void put(T o) throws InterruptedException {
        synchronized (writer) {
            while (!isEmpty()) {
                writer.wait();
            }

            synchronized (reader) {
                content = o;
                empty = false;
                reader.notify();
            }
        }
    }

    public T take() throws InterruptedException {
        synchronized (reader) {
            while (isEmpty()) {
                reader.wait();
            }

            synchronized (writer) {
                T result = content;
                content = null;
                empty = true;
                writer.notify();
                return result;
            }
        }
    }

    public T take(long timeOut) throws InterruptedException {
        long start = System.currentTimeMillis();

        synchronized (reader) {
            while (isEmpty()) {
                long deltaT = System.currentTimeMillis() - start;
                if (deltaT >= timeOut) {
                    return null;
                }
                reader.wait(timeOut - deltaT);
            }

            synchronized (writer) {
                T result = content;
                content = null;
                empty = true;
                writer.notify();
                return result;
            }
        }
    }

    public T read() throws InterruptedException {
        synchronized (reader) {
            while (isEmpty()) {
                reader.wait();
            }

            reader.notify();
            return content;
        }
    }

    public boolean tryPut(T toPut) {
        if (!isEmpty()) {
            return false;
        }

        synchronized (reader) {
            content = toPut;
            empty = false;
            reader.notify();
            return true;
        }
    }

    public T tryTake() {
        if (!isEmpty()) {
            synchronized (writer) {
                T result = content;

                empty = true;
                content = null;
                writer.notify();

                return result;
            }
        }

        return null;
    }

    public T swap(T toSwap) throws InterruptedException {
        synchronized (reader) {
            while (isEmpty()) {
                reader.wait();
            }

            T result = content;

            content = toSwap;
            empty = false;
            reader.notify();

            return result;
        }
    }

    public void clear() {
        synchronized (writer) {
            content = null;
            empty = true;
            writer.notify();
        }
    }

    public void overWrite(T o) {
        //todo: lol?! mind blown
    }
}
