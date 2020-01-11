package decaf.driver.error;

import decaf.frontend.tree.Pos;

public class BadAbstractClassError extends DecafError {

    private String name;

    public BadAbstractClassError(Pos pos, String name) {
        super(pos);
        this.name = name;
    }

    @Override
    protected String getErrMsg() {
        return "'" + this.name + "' is not abstract and does not override all abstract methods";
    }
}
