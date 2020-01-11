package decaf.backend.opt;

import decaf.backend.dataflow.CFG;
import decaf.backend.dataflow.LivenessAnalyzer;
import decaf.lowlevel.instr.Temp;
import decaf.lowlevel.tac.TacInstr;

import java.util.Optional;
import java.util.TreeSet;
import java.util.function.Consumer;


public class AlivenessAnalyzer implements Consumer<CFG<TacInstr>> {

    @Override
    public void accept(CFG<TacInstr> graph) {
        new LivenessAnalyzer<TacInstr>().accept(graph);

        for (var bb : graph.nodes) {
            var liveOut = new TreeSet<>(bb.liveOut);
            var it = bb.backwardIterator();
            while (it.hasNext()) {
                var loc = it.next();
                loc.liveOut = new TreeSet<>(liveOut);

                if (loc.instr instanceof TacInstr.DirectCall || loc.instr instanceof TacInstr.IndirectCall) {
                    if (loc.instr.dsts.length > 0 && !loc.liveOut.contains(loc.instr.dsts[0])) {
                        loc.instr.dsts = new Temp[0];
                        if (loc.instr instanceof TacInstr.DirectCall) {
                            ((TacInstr.DirectCall) loc.instr).dst = Optional.empty();
                        } else { // loc.instr instanceof TacInstr.IndirectCall
                            ((TacInstr.IndirectCall) loc.instr).dst = Optional.empty();
                        }
                    }
                } else if (loc.instr.dsts.length > 0 && !loc.liveOut.contains(loc.instr.dsts[0])) {
                    bb.locs.remove(loc);
                    continue;
                }

                // Order is important here, because in an instruction, one temp can be both read and written, e.g.
                // in `_T1 = _T1 + _T2`, `_T1` must be alive before execution.
                liveOut.removeAll(loc.instr.getWritten());
                liveOut.addAll(loc.instr.getRead());
                loc.liveIn = new TreeSet<>(liveOut);
            }
            // assert liveIn == bb.liveIn
        }
    }
}
