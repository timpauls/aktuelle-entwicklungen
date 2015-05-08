package ueb04;

import ueb02.MVar;

class Chan<T> {
    private MVar<MVar<ChanElem<T>>> read, write;

    private class ChanElem<T> {

        private T value;
        private MVar<ChanElem<T>> next;

        public ChanElem(T v, MVar<ChanElem<T>> n) {
            value = v;
            next = n;
        }

        public T value() {
            return value;
        }

        public MVar<ChanElem<T>> next() {
            return next;
        }
    }

    public Chan() throws InterruptedException {
        MVar<ChanElem<T>> hole = new MVar<ChanElem<T>>();
        read = new MVar<MVar<ChanElem<T>>>(hole);
        write = new MVar<MVar<ChanElem<T>>>(hole);
    }

    public T read() throws InterruptedException {
        MVar<ChanElem<T>> rEnd = read.take();
        ChanElem<T> item = rEnd.take();
        read.put(item.next());
        return item.value();
    }

    public void write(T o) throws InterruptedException {
        MVar<ChanElem<T>> newHole = new MVar<ChanElem<T>>();
        MVar<ChanElem<T>> oldHole = write.take();
        oldHole.put(new ChanElem<T>(o, newHole));
        write.put(newHole);
    }

    public boolean isEmpty() throws InterruptedException {
        return read.read() == write.read();
    }

    public void unGet(T o) throws InterruptedException {
        MVar<ChanElem<T>> oldFirst = read.take();
        MVar<ChanElem<T>> newFirst = new MVar<ChanElem<T>>();

        ChanElem<T> elem = new ChanElem<T>(o, oldFirst);
        newFirst.put(elem);

        read.put(newFirst);
    }
}