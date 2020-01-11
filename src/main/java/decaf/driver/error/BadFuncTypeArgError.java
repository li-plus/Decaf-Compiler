package decaf.driver.error;

import decaf.frontend.tree.Pos;

public class BadFuncTypeArgError extends DecafError {

    public BadFuncTypeArgError(Pos pos) {
        super(pos);
    }

    @Override
    protected String getErrMsg() {
        return "arguments in function type must be non-void known type";
    }
}
