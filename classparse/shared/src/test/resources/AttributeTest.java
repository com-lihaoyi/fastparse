import java.io.IOException;

public class AttributeTest<T> {
    public final static int CONST_VAL = 42;

    public class InnerClass {
        public int innerField;
    }

    @Deprecated
    public T method() throws IOException {
        InnerClass innerClass = new InnerClass();
        innerClass.innerField = CONST_VAL;
        throw new IOException();
    }
}