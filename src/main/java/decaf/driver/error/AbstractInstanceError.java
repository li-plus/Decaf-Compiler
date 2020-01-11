package decaf.driver.error;

import decaf.frontend.tree.Pos;

public class AbstractInstanceError extends DecafError {

    private String name;

    public AbstractInstanceError(Pos pos, String name) {
        super(pos);
        this.name = name;
    }

    @Override
    protected String getErrMsg() {
        return "cannot instantiate abstract class '" + this.name + "'";
    }
}
