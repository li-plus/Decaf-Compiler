package decaf.backend.opt;

import decaf.backend.dataflow.BasicBlock;
import decaf.backend.dataflow.CFG;
import decaf.lowlevel.instr.Temp;
import decaf.lowlevel.tac.TacInstr;
import org.apache.commons.lang3.tuple.Pair;

import java.util.HashMap;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Consumer;


public class CopyPropagation implements Consumer<CFG<TacInstr>> {

    @Override
    public void accept(CFG<TacInstr> graph) {
        for (var bb : graph.nodes) {
            computeGenAndKillFor(bb);
            bb.in = new TreeSet<>();
            bb.out = new TreeSet<>(bb.gen);
        }

        var changed = true;
        while (changed) {
            changed = false;
            for (var bb : graph.nodes) {
                var newIn = new TreeSet<>();
                for (var prev : graph.getPrev(bb.id)) {
                    // meet function
                    for (var object : graph.getBlock(prev).out) {
                        var entry = (Pair<Temp, Temp>) object;
                        boolean isAll = true;
                        for (var prevCheck : graph.getPrev(bb.id)) {
                            if (!graph.getBlock(prevCheck).out.contains(entry)) {
                                isAll = false;
                                break;
                            }
                        }
                        if (isAll) {
                            newIn.add(entry);
                        }
                    }
                }
                if (!newIn.equals(bb.in)) {
                    changed = true;
                    // propagation function
                    bb.in = newIn;
                    bb.out = new TreeSet<>();
                    bb.out.addAll(bb.in);
                    bb.out.removeIf((Object object) -> bb.kill.contains(((Pair<Temp, Temp>) object).getKey()));
                    bb.out.addAll(bb.gen);
                }
            }
        }

        // do optimization
        for (var bb : graph.nodes) {
            var copyPropMap = new HashMap<Temp, Temp>();
            for (var object : bb.in) {
                var copyEntry = (Pair<Temp, Temp>) object;
                copyPropMap.put(copyEntry.getKey(), copyEntry.getValue());
            }

            for (var loc : bb.locs) {
                // update copy prop map
                if (loc.instr.dsts.length > 0) {
                    var dst = loc.instr.dsts[0];

                    var newCopyPropMap = new HashMap<>(copyPropMap);
                    for (var entry : copyPropMap.entrySet()) {
                        if (entry.getValue() == dst) {
                            newCopyPropMap.remove(entry.getKey(), entry.getValue());
                        }
                    }
                    copyPropMap = newCopyPropMap;

                    if (loc.instr instanceof TacInstr.Assign && loc.instr.srcs[0].index != loc.instr.dsts[0].index) {
                        copyPropMap.put(dst, loc.instr.srcs[0]);
                    }
                }

                for (var idxSrc = 0; idxSrc < loc.instr.srcs.length; idxSrc++) {
                    while (copyPropMap.containsKey(loc.instr.srcs[idxSrc])) {
                        loc.instr.srcs[idxSrc] = copyPropMap.get(loc.instr.srcs[idxSrc]);
                    }
                }
                loc.instr.accept(new UpdateTempsVisitor());
            }
        }
    }

    private void computeGenAndKillFor(BasicBlock<TacInstr> bb) {
        bb.gen = new TreeSet<>();
        bb.kill = new TreeSet<>();

        for (var loc : bb) {
            if (loc.instr.dsts.length > 0) {
                var dst = loc.instr.dsts[0];
                bb.kill.addAll(loc.instr.getWritten());

                var oldGen = Set.copyOf(bb.gen);
                for (var object : oldGen) {
                    var entry = (Pair<Temp, Temp>) object;
                    if (entry.getKey() == dst || entry.getValue() == dst) {
                        bb.gen.remove(Pair.of(entry.getKey(), entry.getValue()));
                    }
                }
                if (loc.instr instanceof TacInstr.Assign) {
                    bb.gen.add(Pair.of(dst, loc.instr.srcs[0]));
                }
            }
        }
    }

    class UpdateTempsVisitor implements TacInstr.Visitor {

        @Override
        public void visitAssign(TacInstr.Assign instr) {
            instr.dst = instr.dsts[0];
            instr.src = instr.srcs[0];
        }

        @Override
        public void visitLoadVTbl(TacInstr.LoadVTbl instr) {
            instr.dst = instr.dsts[0];
        }

        @Override
        public void visitLoadImm4(TacInstr.LoadImm4 instr) {
            instr.dst = instr.dsts[0];
        }

        @Override
        public void visitLoadStrConst(TacInstr.LoadStrConst instr) {
            instr.dst = instr.dsts[0];
        }

        @Override
        public void visitUnary(TacInstr.Unary instr) {
            instr.dst = instr.dsts[0];
            instr.operand = instr.srcs[0];
        }

        @Override
        public void visitBinary(TacInstr.Binary instr) {
            instr.dst = instr.dsts[0];
            instr.lhs = instr.srcs[0];
            instr.rhs = instr.srcs[1];
        }

        @Override
        public void visitCondBranch(TacInstr.CondBranch instr) {
            instr.cond = instr.srcs[0];
        }

        @Override
        public void visitReturn(TacInstr.Return instr) {
            instr.value = (instr.srcs.length > 0) ? Optional.of(instr.srcs[0]) : Optional.empty();
        }

        @Override
        public void visitParm(TacInstr.Parm instr) {
            instr.value = instr.srcs[0];
        }

        @Override
        public void visitIndirectCall(TacInstr.IndirectCall instr) {
            instr.dst = (instr.dsts.length > 0) ? Optional.of(instr.dsts[0]) : Optional.empty();
            instr.entry = instr.srcs[0];
        }

        @Override
        public void visitDirectCall(TacInstr.DirectCall instr) {
            instr.dst = (instr.dsts.length > 0) ? Optional.of(instr.dsts[0]) : Optional.empty();
        }

        @Override
        public void visitMemory(TacInstr.Memory instr) {
            instr.dst = instr.op.equals(TacInstr.Memory.Op.LOAD) ? instr.dsts[0] : instr.srcs[0];
            instr.base = instr.op.equals(TacInstr.Memory.Op.LOAD) ? instr.srcs[0] : instr.srcs[1];
        }
    }
}
