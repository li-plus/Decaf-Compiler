package decaf.frontend.scope;

import decaf.frontend.symbol.LambdaSymbol;
import decaf.frontend.type.Type;

import java.util.ArrayList;
import java.util.List;

public class LambdaScope extends Scope {

    public LambdaScope(LocalScope parent) {
        super(Kind.LAMBDA);
        parent.nestedLocalScopes().add(this);
    }

    @Override
    public boolean isLambdaScope() {
        return true;
    }

    public LambdaSymbol owner;

    public LocalScope nested;

    public List<Type> returnTypes = new ArrayList<>();
}
