package decaf.driver.error;

import decaf.frontend.tree.Pos;

public class BadBlockReturnTypeError extends DecafError {

    public BadBlockReturnTypeError(Pos pos) {
        super(pos);
    }

    @Override
    protected String getErrMsg() {
        return "incompatible return types in blocked expression";
    }
}
