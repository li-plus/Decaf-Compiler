package decaf.frontend.typecheck;

import decaf.driver.Config;
import decaf.driver.Phase;
import decaf.driver.error.*;
import decaf.frontend.scope.ScopeStack;
import decaf.frontend.symbol.ClassSymbol;
import decaf.frontend.symbol.MethodSymbol;
import decaf.frontend.symbol.VarSymbol;
import decaf.frontend.tree.Tree;
import decaf.frontend.type.*;
import decaf.lowlevel.log.IndentPrinter;
import decaf.printing.PrettyScope;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * The typer phase: type check abstract syntax tree and annotate nodes with inferred (and checked) types.
 */
public class Typer extends Phase<Tree.TopLevel, Tree.TopLevel> implements TypeLitVisited {

    public Typer(Config config) {
        super("typer", config);
    }

    @Override
    public Tree.TopLevel transform(Tree.TopLevel tree) {
        var ctx = new ScopeStack(tree.globalScope);
        tree.accept(this, ctx);
        return tree;
    }

    @Override
    public void onSucceed(Tree.TopLevel tree) {
        if (config.target.equals(Config.Target.PA2)) {
            var printer = new PrettyScope(new IndentPrinter(config.output));
            printer.pretty(tree.globalScope);
            printer.flush();
        }
    }

    @Override
    public void visitTopLevel(Tree.TopLevel program, ScopeStack ctx) {
        for (var clazz : program.classes) {
            clazz.accept(this, ctx);
        }
    }

    @Override
    public void visitClassDef(Tree.ClassDef clazz, ScopeStack ctx) {
        ctx.open(clazz.symbol.scope);
        for (var field : clazz.fields) {
            field.accept(this, ctx);
        }
        ctx.close();
    }

    @Override
    public void visitMethodDef(Tree.MethodDef method, ScopeStack ctx) {
        ctx.open(method.symbol.scope);
        if (method.body.isPresent()) {
            method.body.get().accept(this, ctx);
            if (!method.symbol.type.returnType.isVoidType() && !method.body.get().returns) {
                issue(new MissingReturnError(method.body.get().pos));
            }
        }
        ctx.close();
    }

    /**
     * To determine if a break statement is legal or not, we need to know if we are inside a loop, i.e.
     * loopLevel {@literal >} 1?
     * <p>
     * Increase this counter when entering a loop, and decrease it when leaving a loop.
     */
    private int loopLevel = 0;

    @Override
    public void visitBlock(Tree.Block block, ScopeStack ctx) {
        ctx.open(block.scope);
        for (var stmt : block.stmts) {
            stmt.accept(this, ctx);
        }
        ctx.close();
        block.returns = !block.stmts.isEmpty() && block.stmts.get(block.stmts.size() - 1).returns;
    }

    @Override
    public void visitAssign(Tree.Assign stmt, ScopeStack ctx) {
        stmt.lhs.accept(this, ctx);
        stmt.rhs.accept(this, ctx);
        var lt = stmt.lhs.type;
        var rt = stmt.rhs.type;

        if (lt.noError()) {
            if (!rt.subtypeOf(lt)) {
                issue(new IncompatBinOpError(stmt.pos, lt.toString(), "=", rt.toString()));
            }
            if (ctx.currentLambda().isPresent() && stmt.lhs instanceof Tree.VarSel) {
                //  int a = 1;
                //  var f = fun() { a = 2; }; // bad
                var lhs = (Tree.VarSel) stmt.lhs;
                if (ctx.currentLambda().get().capturedVars.contains(lhs.name)) {
                    issue(new CapturedVarAssignmentError(stmt.pos));
                }
            }
            if (stmt.lhs instanceof Tree.VarSel) {
                //  var a = fun() { var b = Main.f; }; // bad
                var lhs = (Tree.VarSel) stmt.lhs;
                var symbol = ctx.currentClass().scope.lookup(lhs.name);
                if (symbol.isPresent() && symbol.get().isMethodSymbol()) {
                    issue(new MemberMethodAssignmentError(stmt.pos, symbol.get().name));
                }
            }
        }
    }

    @Override
    public void visitExprEval(Tree.ExprEval stmt, ScopeStack ctx) {
        stmt.expr.accept(this, ctx);
    }

    @Override
    public void visitIf(Tree.If stmt, ScopeStack ctx) {
        checkTestExpr(stmt.cond, ctx);
        stmt.trueBranch.accept(this, ctx);
        stmt.falseBranch.ifPresent(b -> b.accept(this, ctx));
        // if-stmt returns a value iff both branches return
        stmt.returns = stmt.trueBranch.returns && stmt.falseBranch.isPresent() && stmt.falseBranch.get().returns;
    }

    @Override
    public void visitWhile(Tree.While loop, ScopeStack ctx) {
        checkTestExpr(loop.cond, ctx);
        loopLevel++;
        loop.body.accept(this, ctx);
        loopLevel--;
    }

    @Override
    public void visitFor(Tree.For loop, ScopeStack ctx) {
        ctx.open(loop.scope);
        loop.init.accept(this, ctx);
        checkTestExpr(loop.cond, ctx);
        loop.update.accept(this, ctx);
        loopLevel++;
        for (var stmt : loop.body.stmts) {
            stmt.accept(this, ctx);
        }
        loopLevel--;
        ctx.close();
    }

    @Override
    public void visitBreak(Tree.Break stmt, ScopeStack ctx) {
        if (loopLevel == 0) {
            issue(new BreakOutOfLoopError(stmt.pos));
        }
    }

    @Override
    public void visitReturn(Tree.Return stmt, ScopeStack ctx) {
        var expected = ctx.currentMethod().type.returnType;
        stmt.expr.ifPresent(e -> e.accept(this, ctx));
        var actual = stmt.expr.map(e -> e.type).orElse(BuiltInType.VOID);
        if (ctx.currentLambda().isPresent()) {
            // under lambda expression
            ctx.currentLambda().get().scope.returnTypes.add(actual);
        } else if (actual.noError() && !actual.subtypeOf(expected)) {
            issue(new BadReturnTypeError(stmt.pos, expected.toString(), actual.toString()));
        }
        stmt.returns = stmt.expr.isPresent();
    }

    @Override
    public void visitPrint(Tree.Print stmt, ScopeStack ctx) {
        int i = 0;
        for (var expr : stmt.exprs) {
            expr.accept(this, ctx);
            i++;
            if (expr.type.noError() && !expr.type.isBaseType()) {
                issue(new BadPrintArgError(expr.pos, Integer.toString(i), expr.type.toString()));
            }
        }
    }

    private void checkTestExpr(Tree.Expr expr, ScopeStack ctx) {
        expr.accept(this, ctx);
        if (expr.type.noError() && !expr.type.eq(BuiltInType.BOOL)) {
            issue(new BadTestExpr(expr.pos));
        }
    }

    // Expressions

    @Override
    public void visitIntLit(Tree.IntLit that, ScopeStack ctx) {
        that.type = BuiltInType.INT;
    }

    @Override
    public void visitBoolLit(Tree.BoolLit that, ScopeStack ctx) {
        that.type = BuiltInType.BOOL;
    }

    @Override
    public void visitStringLit(Tree.StringLit that, ScopeStack ctx) {
        that.type = BuiltInType.STRING;
    }

    @Override
    public void visitNullLit(Tree.NullLit that, ScopeStack ctx) {
        that.type = BuiltInType.NULL;
    }

    @Override
    public void visitReadInt(Tree.ReadInt readInt, ScopeStack ctx) {
        readInt.type = BuiltInType.INT;
    }

    @Override
    public void visitReadLine(Tree.ReadLine readStringExpr, ScopeStack ctx) {
        readStringExpr.type = BuiltInType.STRING;
    }

    @Override
    public void visitUnary(Tree.Unary expr, ScopeStack ctx) {
        expr.operand.accept(this, ctx);
        var t = expr.operand.type;
        if (t.noError() && !compatible(expr.op, t)) {
            // Only report this error when the operand has no error, to avoid nested errors flushing.
            issue(new IncompatUnOpError(expr.pos, Tree.opStr(expr.op), t.toString()));
        }

        // Even when it doesn't type check, we could make a fair guess based on the operator kind.
        // Let's say the operator is `-`, then one possibly wants an integer as the operand.
        // Once he/she fixes the operand, according to our type inference rule, the whole unary expression
        // must have type int! Thus, we simply _assume_ it has type int, rather than `NoType`.
        expr.type = resultTypeOf(expr.op);
    }

    @Override
    public void visitLambdaExpr(Tree.LambdaExpr expr, ScopeStack ctx) {
        ctx.open(expr.symbol.scope);
        ctx.open(expr.symbol.scope.nested);
        expr.body.accept(this, ctx);
        expr.symbol.type.returnType = expr.body.type;
        expr.type = expr.symbol.type;
        ctx.close();
        ctx.close();
    }

    @Override
    public void visitLambdaBlock(Tree.LambdaBlock expr, ScopeStack ctx) {
        ctx.open(expr.symbol.scope);
        expr.body.accept(this, ctx);
        ctx.close();

        boolean isAllVoidReturns = true;
        for (var returnType : expr.symbol.scope.returnTypes) {
            if (!returnType.eq(BuiltInType.VOID)) {
                isAllVoidReturns = false;
                break;
            }
        }
        if (!isAllVoidReturns && !expr.body.returns) {
            issue(new MissingReturnError(expr.body.pos));
        }

        // find the return types
        expr.symbol.type.returnType = boundType(expr.symbol.scope.returnTypes, true);
        if (expr.symbol.type.returnType.hasError()) {
            issue(new BadBlockReturnTypeError(expr.body.pos));
        }
        expr.type = expr.symbol.type;
    }

    private Type boundType(List<Type> types, boolean upper) {
        // no return statement
        if (types.isEmpty()) {
            return BuiltInType.VOID;
        }

        Type firstType = BuiltInType.NULL;
        for (var type : types) {
            if (!type.eq(BuiltInType.NULL)) {
                firstType = type;
                break;
            }
        }

        if (firstType.eq(BuiltInType.NULL)) {
            return BuiltInType.NULL;
        } else if (firstType.isVoidType() || firstType.isBaseType() || firstType.isArrayType()) {
            for (var type : types) {
                if (!type.eq(firstType)) {
                    return BuiltInType.ERROR;
                }
            }
            return firstType;
        } else if (firstType.isClassType()) {
            if (upper) {
                var commonParent = Optional.of((ClassType) firstType);
                while (commonParent.isPresent()) {
                    boolean isFound = true;
                    for (var type : types) {
                        if (!type.subtypeOf(commonParent.get())) {
                            isFound = false;
                            break;
                        }
                    }
                    if (isFound) {
                        return commonParent.get();
                    }
                    commonParent = commonParent.get().superType;
                }
                return BuiltInType.ERROR;
            } else {
                var commonChild = firstType;
                for (var type : types) {
                    if (type.subtypeOf(commonChild)) {
                        commonChild = type;
                    }
                    if (!commonChild.subtypeOf(type)) {
                        return BuiltInType.ERROR;
                    }
                }
                return commonChild;
            }
        } else if (firstType.isFuncType()) {
            var firstFuncType = (FunType) firstType;

            // check signatures and build argument type matrix
            var argTypeMat = new ArrayList<List<Type>>();
            var returnTypes = new ArrayList<Type>();

            for (Type type : types) {
                if (!type.isFuncType()) {
                    return BuiltInType.ERROR;
                }
                var funType = (FunType) type;
                if (funType.argTypes.size() != firstFuncType.argTypes.size()) {
                    return BuiltInType.ERROR;
                }
                argTypeMat.add(funType.argTypes);
                returnTypes.add(funType.returnType);
            }
            // upper bound of return types
            var returnType = boundType(returnTypes, upper);
            if (returnType.hasError()) {
                return BuiltInType.ERROR;
            }

            // lower bounds of arguments
            var commonArgTypes = new ArrayList<Type>();
            for (var positionalArgType : transpose(argTypeMat)) {
                var commonArgType = boundType(positionalArgType, !upper);
                if (commonArgType.hasError()) {
                    return BuiltInType.ERROR;
                }
                commonArgTypes.add(commonArgType);
            }
            return new FunType(returnType, commonArgTypes);
        }
        return BuiltInType.ERROR;
    }

    private static <E> List<List<E>> transpose(ArrayList<List<E>> matrixIn) {
        List<List<E>> matrixOut = new ArrayList<>();
        if (!matrixIn.isEmpty()) {
            int newCols = matrixIn.get(0).size();
            for (int i = 0; i < newCols; i++) {
                List<E> col = new ArrayList<>();
                for (List<E> row : matrixIn) {
                    col.add(row.get(i));
                }
                matrixOut.add(col);
            }
        }
        return matrixOut;
    }

    public boolean compatible(Tree.UnaryOp op, Type operand) {
        return switch (op) {
            case NEG -> operand.eq(BuiltInType.INT); // if e : int, then -e : int
            case NOT -> operand.eq(BuiltInType.BOOL); // if e : bool, then !e : bool
        };
    }

    public Type resultTypeOf(Tree.UnaryOp op) {
        return switch (op) {
            case NEG -> BuiltInType.INT;
            case NOT -> BuiltInType.BOOL;
        };
    }

    @Override
    public void visitBinary(Tree.Binary expr, ScopeStack ctx) {
        expr.lhs.accept(this, ctx);
        expr.rhs.accept(this, ctx);
        var t1 = expr.lhs.type;
        var t2 = expr.rhs.type;
        if (t1.noError() && t2.noError() && !compatible(expr.op, t1, t2)) {
            issue(new IncompatBinOpError(expr.pos, t1.toString(), Tree.opStr(expr.op), t2.toString()));
        }
        expr.type = resultTypeOf(expr.op);
    }

    public boolean compatible(Tree.BinaryOp op, Type lhs, Type rhs) {
        if (op.compareTo(Tree.BinaryOp.ADD) >= 0 && op.compareTo(Tree.BinaryOp.MOD) <= 0) { // arith
            // if e1, e2 : int, then e1 + e2 : int
            return lhs.eq(BuiltInType.INT) && rhs.eq(BuiltInType.INT);
        }

        if (op.equals(Tree.BinaryOp.AND) || op.equals(Tree.BinaryOp.OR)) { // logic
            // if e1, e2 : bool, then e1 && e2 : bool
            return lhs.eq(BuiltInType.BOOL) && rhs.eq(BuiltInType.BOOL);
        }

        if (op.equals(Tree.BinaryOp.EQ) || op.equals(Tree.BinaryOp.NE)) { // eq
            // if e1 : T1, e2 : T2, T1 <: T2 or T2 <: T1, then e1 == e2 : bool
            return lhs.subtypeOf(rhs) || rhs.subtypeOf(lhs);
        }

        // compare
        // if e1, e2 : int, then e1 > e2 : bool
        return lhs.eq(BuiltInType.INT) && rhs.eq(BuiltInType.INT);
    }

    public Type resultTypeOf(Tree.BinaryOp op) {
        if (op.compareTo(Tree.BinaryOp.ADD) >= 0 && op.compareTo(Tree.BinaryOp.MOD) <= 0) { // arith
            return BuiltInType.INT;
        }
        return BuiltInType.BOOL;
    }

    @Override
    public void visitNewArray(Tree.NewArray expr, ScopeStack ctx) {
        expr.elemType.accept(this, ctx);
        expr.length.accept(this, ctx);
        var et = expr.elemType.type;
        var lt = expr.length.type;

        if (et.isVoidType()) {
            issue(new BadArrElementError(expr.elemType.pos));
            expr.type = BuiltInType.ERROR;
        } else {
            expr.type = new ArrayType(et);
        }

        if (lt.noError() && !lt.eq(BuiltInType.INT)) {
            issue(new BadNewArrayLength(expr.length.pos));
        }
    }

    @Override
    public void visitNewClass(Tree.NewClass expr, ScopeStack ctx) {
        var clazz = ctx.lookupClass(expr.clazz.name);
        if (clazz.isPresent()) {
            expr.symbol = clazz.get();
            expr.type = expr.symbol.type;
            if (expr.symbol.modifiers.isAbstract()) {
                issue(new AbstractInstanceError(expr.pos, expr.symbol.name));
            }
        } else {
            issue(new ClassNotFoundError(expr.pos, expr.clazz.name));
            expr.type = BuiltInType.ERROR;
        }
    }

    @Override
    public void visitThis(Tree.This expr, ScopeStack ctx) {
        if (ctx.currentMethod().isStatic()) {
            issue(new ThisInStaticFuncError(expr.pos));
        }
        expr.type = ctx.currentClass().type;
    }

    private boolean allowClassNameVar = false;

    @Override
    public void visitVarSel(Tree.VarSel expr, ScopeStack ctx) {
        if (expr.receiver.isEmpty()) {
            // Variable, which should be complicated since a legal variable could refer to a local var,
            // a visible member var, and a class name.

            //  VarSel
            //      <none>
            //      MyMethod / MyClass / MyMemberVar / MyLocalVar / MyLambda
            var symbol = ctx.lookupBefore(expr.name, expr.pos);
            if (symbol.isPresent() && !definingVarNames.contains(symbol.get().name)) {
                expr.symbol = symbol.get();

                if (symbol.get().isVarSymbol()) {
                    var var = (VarSymbol) symbol.get();
                    expr.type = var.type;
                    if (var.isMemberVar()) {
                        if (ctx.currentMethod().isStatic()) {
                            issue(new RefNonStaticError(expr.pos, ctx.currentMethod().name, expr.name));
                        } else {
                            expr.setThis();
                            expr.receiver.get().accept(this, ctx);
                        }
                    }
                    if (ctx.currentLambda().isPresent()) {
                        for (int i = ctx.lambdaScopeStack.size() - 1; i > -1; i--) {
                            var lambdaScope = ctx.lambdaScopeStack.get(i);
                            var capturedSymbol = ctx.lookupBefore(expr.name, lambdaScope.owner.pos);
                            if (capturedSymbol.isEmpty()) {
                                break;
                            }
                            if (capturedSymbol.get().isVarSymbol()) {
                                if (((VarSymbol) capturedSymbol.get()).isMemberVar()) {
                                    // capture this
                                    if (!lambdaScope.owner.capturedVars.contains("this")) {
                                        lambdaScope.owner.capturedVars.add("this");
                                    }
                                } else {
                                    // capture local var
                                    if (!lambdaScope.owner.capturedVars.contains(capturedSymbol.get().name)) {
                                        lambdaScope.owner.capturedVars.add(capturedSymbol.get().name);
                                    }
                                }
                            }
                        }
                    }
                    return;
                }

                if (symbol.get().isClassSymbol() && allowClassNameVar) { // special case: a class name
                    var clazz = (ClassSymbol) symbol.get();
                    expr.type = clazz.type;
                    expr.isClassName = true;
                    return;
                }

                if (symbol.get().isMethodSymbol()) {
                    var method = (MethodSymbol) symbol.get();
                    expr.type = method.type;
                    if (ctx.currentMethod().isStatic() && !method.isStatic()) {
                        issue(new RefNonStaticError(expr.pos, ctx.currentMethod().name, expr.name));
                    }
                    if (!method.isStatic()) {
                        expr.setThis();
                    }
                    return;
                }
                // this cannot be reached
            }

            expr.type = BuiltInType.ERROR;
            issue(new UndeclVarError(expr.pos, expr.name));
            return;
        }

        // has receiver
        var receiver = expr.receiver.get();
        allowClassNameVar = true;
        receiver.accept(this, ctx);
        allowClassNameVar = false;
        var rt = receiver.type;
        expr.type = BuiltInType.ERROR;

        if (receiver instanceof Tree.VarSel) {
            var v1 = (Tree.VarSel) receiver;
            if (v1.isClassName) {
                //  VarSel
                //      VarSel
                //          <none>
                //          MyClass
                //      foo
                var clazz = ctx.getClass(v1.name);
                var symbol = clazz.scope.lookup(expr.variable.name);
                if (symbol.isEmpty()) {
                    issue(new FieldNotFoundError(expr.pos, expr.name, ctx.getClass(v1.name).type.toString()));
                    return;
                }
                expr.symbol = symbol.get();
                if (expr.symbol.isVarSymbol()) {
                    // special case like MyClass.foo: report error cannot access field 'foo' from 'class : MyClass'
                    issue(new NotClassFieldError(expr.pos, expr.name, ctx.getClass(v1.name).type.toString()));
                    return;
                }
                if (expr.symbol.isMethodSymbol() && !((MethodSymbol) symbol.get()).isStatic()) {
                    issue(new NotClassFieldError(expr.variable.pos, expr.variable.name, clazz.type.toString()));
                    return;
                }
            }
        }

        if (!rt.noError()) {
            return;
        }

        if (rt.isArrayType() && expr.variable.name.equals("length")) {
            expr.type = new FunType(BuiltInType.INT, new ArrayList<>());
            expr.isArrayLength = true;
            return;
        }

        if (!rt.isClassType()) {
            issue(new NotClassFieldError(expr.pos, expr.name, rt.toString()));
            return;
        }

        //  VarSel
        //      VarSel
        //          <none>
        //          a (ClassType)
        //      b
        var ct = (ClassType) rt;
        var field = ctx.getClass(ct.name).scope.lookup(expr.name);
        if (field.isEmpty()) {
            issue(new FieldNotFoundError(expr.pos, expr.name, ct.toString()));
            return;
        }
        expr.symbol = field.get();
        if (expr.symbol.isVarSymbol()) {
            var var = (VarSymbol) field.get();
            expr.type = var.type;
            if (var.isMemberVar()) {
                expr.symbol = var;
                expr.type = var.type;
                if (!ctx.currentClass().type.subtypeOf(var.getOwner().type)) {
                    // member vars are protected
                    issue(new FieldNotAccessError(expr.pos, expr.name, ct.toString()));
                }
            }
        } else if (expr.symbol.isMethodSymbol()) {
            expr.type = field.get().type;
        } else {
            issue(new NotClassFieldError(expr.pos, expr.name, ct.toString()));
        }
    }

    @Override
    public void visitIndexSel(Tree.IndexSel expr, ScopeStack ctx) {
        expr.array.accept(this, ctx);
        expr.index.accept(this, ctx);
        var at = expr.array.type;
        var it = expr.index.type;

        expr.type = BuiltInType.ERROR;
        if (at.noError()) {
            if (!at.isArrayType()) {
                issue(new NotArrayError(expr.array.pos));
                return;
            }
            expr.type = ((ArrayType) at).elementType;
        }

        if (!it.eq(BuiltInType.INT)) {
            issue(new SubNotIntError(expr.pos));
        }
    }

    @Override
    public void visitCall(Tree.Call expr, ScopeStack ctx) {
        expr.expr.accept(this, ctx);

        expr.type = BuiltInType.ERROR;
        Type rt;
        boolean thisClass = false;

        if (expr.expr instanceof Tree.VarSel) {
            var varSelExpr = (Tree.VarSel) expr.expr;
            if (varSelExpr.receiver.isPresent()) {
                var receiver = varSelExpr.receiver.get();
                rt = receiver.type;

                if (receiver instanceof Tree.VarSel) {
                    var v1 = (Tree.VarSel) receiver;
                    if (v1.isClassName) {
                        // Special case: invoking a static method, like MyClass.foo()
                        typeCall(expr, false, v1.name, ctx, true, false);
                        return;
                    }
                }

                // member method
                if (receiver instanceof Tree.This) {
                    thisClass = true;
                    varSelExpr.setThis();
                    rt = ctx.currentClass().type;
                }
            } else {
                // local var of lambda expression
                rt = BuiltInType.NULL;
            }

            if (rt.noError()) {
                if (varSelExpr.isArrayLength) { // Special case: array.length()
                    if (!expr.args.isEmpty()) {
                        issue(new BadLengthArgError(expr.pos, expr.args.size()));
                    }
                    expr.type = BuiltInType.INT;
                    return;
                }

                if (rt.isClassType()) {
                    typeCall(expr, thisClass, ((ClassType) rt).name, ctx, false, false);
                    return;
                }

                if (rt.eq(BuiltInType.NULL)) {
                    // check lambda args
                    typeCall(expr, false, ctx.currentClass().name, ctx, false, true);
                    return;
                }
            }
        } else {
            if (expr.expr.type.hasError()) {
                return;
            }
            if (!expr.expr.type.isFuncType()) {
                issue(new NotCallableTypeError(expr.pos, expr.expr.type));
                return;
            }
            var funcType = (FunType) expr.expr.type;

            // typing args
            var args = expr.args;
            for (var arg : args) {
                arg.accept(this, ctx);
            }

            // check signature compatibility
            if (funcType.arity() != args.size()) {
                issue(new BadLambdaArgCountError(expr.pos, funcType.arity(), args.size()));
            }
            var iter1 = funcType.argTypes.iterator();
            var iter2 = expr.args.iterator();
            for (int i = 1; iter1.hasNext() && iter2.hasNext(); i++) {
                Type t1 = iter1.next();
                Tree.Expr e = iter2.next();
                Type t2 = e.type;
                if (t2.noError() && !t2.subtypeOf(t1)) {
                    issue(new BadArgTypeError(e.pos, i, t2.toString(), t1.toString()));
                }
            }

            expr.type = funcType.returnType;
        }
    }

    private void typeCall(Tree.Call call, boolean thisClass, String className, ScopeStack ctx, boolean requireStatic, boolean isLambda) {
        if (call.expr instanceof Tree.VarSel) {
            var varSelExpr = (Tree.VarSel) call.expr;
            var clazz = thisClass ? ctx.currentClass() : ctx.getClass(className);

            var symbol = (isLambda
                    ? ctx.lookupBefore(varSelExpr.variable.name, varSelExpr.pos)
                    : clazz.scope.lookup(varSelExpr.variable.name));

            if (symbol.isPresent()) {
                if (symbol.get().isMethodSymbol()) {
                    var method = (MethodSymbol) symbol.get();
                    call.type = method.type.returnType;

                    // typing args
                    var args = call.args;
                    for (var arg : args) {
                        arg.accept(this, ctx);
                    }

                    // check signature compatibility
                    if (method.type.arity() != args.size()) {
                        issue(new BadArgCountError(call.pos, method.name, method.type.arity(), args.size()));
                    }
                    var iter1 = method.type.argTypes.iterator();
                    var iter2 = call.args.iterator();
                    for (int i = 1; iter1.hasNext() && iter2.hasNext(); i++) {
                        Type t1 = iter1.next();
                        Tree.Expr e = iter2.next();
                        Type t2 = e.type;
                        if (t2.noError() && !t2.subtypeOf(t1)) {
                            issue(new BadArgTypeError(e.pos, i, t2.toString(), t1.toString()));
                        }
                    }
                } else if (symbol.get().isVarSymbol()) {
                    var variable = (VarSymbol) symbol.get();
                    if (!variable.type.isFuncType()) {
                        if (varSelExpr.type.noError()) {
                            issue(new NotCallableTypeError(call.pos, varSelExpr.type));
                        }
                        return;
                    }
                    var funcType = (FunType) variable.type;
                    call.type = funcType.returnType;

                    // typing args
                    var args = call.args;
                    for (var arg : args) {
                        arg.accept(this, ctx);
                    }

                    // check signature compatibility
                    if (funcType.arity() != args.size()) {
                        issue(new BadArgCountError(call.pos, variable.name, funcType.arity(), args.size()));
                    }
                    var iter1 = funcType.argTypes.iterator();
                    var iter2 = call.args.iterator();
                    for (int i = 1; iter1.hasNext() && iter2.hasNext(); i++) {
                        Type t1 = iter1.next();
                        Tree.Expr e = iter2.next();
                        Type t2 = e.type;
                        if (t2.noError() && !t2.subtypeOf(t1)) {
                            issue(new BadArgTypeError(e.pos, i, t2.toString(), t1.toString()));
                        }
                    }
                } else {
                    if (varSelExpr.type.noError()) {
                        issue(new NotCallableTypeError(call.pos, varSelExpr.type));
                    }
                }
            }
        }
    }

    @Override
    public void visitClassTest(Tree.ClassTest expr, ScopeStack ctx) {
        expr.obj.accept(this, ctx);
        expr.type = BuiltInType.BOOL;

        if (!expr.obj.type.isClassType()) {
            issue(new NotClassError(expr.obj.type.toString(), expr.pos));
        }
        var clazz = ctx.lookupClass(expr.is.name);
        if (clazz.isEmpty()) {
            issue(new ClassNotFoundError(expr.pos, expr.is.name));
        } else {
            expr.symbol = clazz.get();
        }
    }

    @Override
    public void visitClassCast(Tree.ClassCast expr, ScopeStack ctx) {
        expr.obj.accept(this, ctx);

        if (!expr.obj.type.isClassType()) {
            issue(new NotClassError(expr.obj.type.toString(), expr.pos));
        }

        var clazz = ctx.lookupClass(expr.to.name);
        if (clazz.isEmpty()) {
            issue(new ClassNotFoundError(expr.pos, expr.to.name));
            expr.type = BuiltInType.ERROR;
        } else {
            expr.symbol = clazz.get();
            expr.type = expr.symbol.type;
        }
    }

    @Override
    public void visitLocalVarDef(Tree.LocalVarDef stmt, ScopeStack ctx) {
        if (stmt.initVal.isEmpty()) {
            return;
        }

        var initVal = stmt.initVal.get();

        definingVarNames.add(stmt.symbol.name);
        initVal.accept(this, ctx);
        definingVarNames.remove(definingVarNames.size() - 1);

        if (stmt.symbol.type.eq(BuiltInType.NULL)) {
            // var x = 1;
            stmt.symbol.type = initVal.type;
            if (initVal.type == BuiltInType.VOID) {
                issue(new BadVarTypeError(stmt.symbol.pos, stmt.name));
                stmt.symbol.type = BuiltInType.ERROR;
            }
        }
        var lt = stmt.symbol.type;
        var rt = initVal.type;
        if (lt.noError() && !rt.subtypeOf(lt)) {
            issue(new IncompatBinOpError(stmt.assignPos, lt.toString(), "=", rt.toString()));
        }
    }

    private List<String> definingVarNames = new ArrayList<>();
}
