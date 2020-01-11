package decaf.frontend.symbol;

import decaf.frontend.scope.LambdaScope;
import decaf.frontend.tree.Pos;
import decaf.frontend.type.FunType;
import decaf.lowlevel.instr.Temp;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public final class LambdaSymbol extends Symbol {

    public FunType type;

    public final LambdaScope scope;

    public List<String> capturedVars = new ArrayList<>();

    public HashMap<String, Temp> capturedTemps = new HashMap<>();

    public LambdaSymbol(String name, FunType type, LambdaScope scope, Pos pos) {
        super(name, type, pos);
        this.type = type;
        this.scope = scope;
        scope.owner = this;
    }

    @Override
    public LambdaScope domain() {
        return (LambdaScope) definedIn;
    }

    @Override
    public boolean isLambdaSymbol() {
        return true;
    }

    @Override
    protected String str() {
        return String.format("function %s : %s", name, type);
    }
}
