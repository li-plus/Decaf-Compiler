package decaf.driver.error;

import decaf.frontend.tree.Pos;
import decaf.frontend.type.Type;

public class NotCallableTypeError extends DecafError {

    private Type type;

    public NotCallableTypeError(Pos pos, Type type) {
        super(pos);
        this.type = type;
    }

    @Override
    protected String getErrMsg() {
        return this.type + " is not a callable type";
    }
}
