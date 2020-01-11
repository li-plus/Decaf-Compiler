package decaf.frontend.tacgen;

import decaf.driver.Config;
import decaf.driver.Phase;
import decaf.frontend.tree.Pos;
import decaf.frontend.tree.Tree;
import decaf.lowlevel.instr.Temp;
import decaf.lowlevel.label.Label;
import decaf.lowlevel.tac.*;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;

/**
 * The tacgen phase: translate an abstract syntax tree to TAC IR.
 */
public class TacGen extends Phase<Tree.TopLevel, TacProg> implements TacEmitter {

    public static final String globalVTable = "__internal_global";

    public static String wrapMethod(String method) {
        return "__internal_" + method;
    }

    public static String lambdaName(Pos pos) {
        return "lambda_" + pos.line + "_" + pos.column;
    }

    public static final String arrayLength = "length";

    public static ProgramWriter pw;

    public TacGen(Config config) {
        super("tacgen", config);
    }

    @Override
    public TacProg transform(Tree.TopLevel tree) {
        // Create class info.
        var info = new ArrayList<ClassInfo>();
        for (var clazz : tree.classes) {
            info.add(clazz.symbol.getInfo());
        }
        pw = new ProgramWriter(info);

        // Step 1: create virtual tables.
        pw.visitVTables();

        // Step 2: emit tac instructions for every method.
        var lengthMv = TacGen.pw.visitFunc(TacGen.globalVTable, TacGen.arrayLength, 1);
        lengthMv.visitReturn(lengthMv.visitLoadFrom(lengthMv.getArgTemp(0), 4));
        lengthMv.visitEnd();

        for (var clazz : tree.classes) {
            for (var method : clazz.methods()) {
                var numArgs = method.params.size() + 1;

                FuncVisitor mv = pw.visitFunc(clazz.name, TacGen.wrapMethod(method.name), numArgs);

                var args = new ArrayList<Temp>();
                for (int i = 1; i < numArgs; i++) {
                    args.add(mv.getArgTemp(i));
                }

                var needReturn = !method.returnType.type.isVoidType();
                Temp returnVal;

                if (method.symbol.isMain()) {
                    mv.visitRaw(new TacInstr.DirectCall(new Label(Label.Kind.FUNC, "main")));
                } else {
                    if (!method.isStatic()) {
                        var object = mv.visitLoadFrom(mv.getArgTemp(0), 4);
                        returnVal = mv.visitMemberCall(object, clazz.name, method.name, args, needReturn);
                    } else {
                        returnVal = mv.visitStaticCall(clazz.name, method.name, args, needReturn);
                    }
                    if (needReturn) {
                        mv.visitReturn(returnVal);
                    }
                }
                mv.visitEnd();
            }
        }

        for (var clazz : tree.classes) {
            for (var method : clazz.methods()) {
                FuncVisitor mv;
                if (method.symbol.isMain()) {
                    mv = pw.visitMainMethod();
                } else {
                    // Remember calling convention: pass `this` (if non-static) as an extra argument, via reversed temps.
                    var numArgs = method.params.size();
                    var i = 0;
                    if (!method.isStatic()) {
                        numArgs++;
                        i++;
                    }

                    mv = pw.visitFunc(clazz.name, method.name, numArgs);
                    for (var param : method.params) {
                        param.symbol.temp = mv.getArgTemp(i);
                        i++;
                    }
                }

                method.body.ifPresent(objects -> objects.accept(this, mv));
                mv.visitEnd();
            }
        }

        return pw.visitEnd();
    }

    @Override
    public void onSucceed(TacProg program) {
        if (config.target.equals(Config.Target.PA3)) {
            // First dump the tac program to file,
            var path = config.dstPath.resolve(config.getSourceBaseName() + ".tac");
            try {
                var printer = new PrintWriter(path.toFile());
                program.printTo(printer);
                printer.close();
            } catch (FileNotFoundException e) {
                e.printStackTrace();
            }

            // and then execute it using our simulator.
            var simulator = new Simulator(System.in, config.output);
            simulator.execute(program);
        }
    }
}
