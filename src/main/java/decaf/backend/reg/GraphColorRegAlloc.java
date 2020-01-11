package decaf.backend.reg;

import decaf.backend.asm.AsmEmitter;
import decaf.backend.asm.HoleInstr;
import decaf.backend.asm.SubroutineEmitter;
import decaf.backend.asm.SubroutineInfo;
import decaf.backend.dataflow.BasicBlock;
import decaf.backend.dataflow.CFG;
import decaf.backend.dataflow.LivenessAnalyzer;
import decaf.backend.dataflow.Loc;
import decaf.lowlevel.Mips;
import decaf.lowlevel.instr.PseudoInstr;
import decaf.lowlevel.instr.Reg;
import decaf.lowlevel.instr.Temp;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;


public final class GraphColorRegAlloc extends RegAlloc {

    InterferenceGraph regGraph;

    HashMap<Integer, Integer> tempToNodeMap; // temp to node

    public GraphColorRegAlloc(AsmEmitter emitter) {
        super(emitter);
        for (var reg : emitter.allocatableRegs) {
            reg.used = false;
        }
    }

    @Override
    public void accept(CFG<PseudoInstr> graph, SubroutineInfo info) {

        if (graph.nodes.isEmpty()) {
            return;
        }

        tempToNodeMap = new HashMap<>();

        regGraph = new InterferenceGraph();

        for (var reg : emitter.allocatableRegs) {
            reg.occupied = false;
        }

        var subEmitter = emitter.emitSubroutine(info);

        // load all arguments from stack to registers
        for (var argIndex = 0; argIndex < info.numArg; argIndex++) {
            addNodeIfAbsent(new Temp(argIndex));
            if (argIndex < Mips.argRegs.length) {
                graph.nodes.get(0).locs.add(argIndex, new Loc<>(new Mips.Move(new Temp(argIndex), Mips.argRegs[argIndex])));
            } else {
                var offset = 4 * argIndex;
                graph.nodes.get(0).locs.add(argIndex, new Loc<>(new Mips.LoadWord(new Temp(argIndex), Mips.SP, offset)));
            }
        }

        new LivenessAnalyzer<>().accept(graph);

        // construct interference graph
        for (var bb : graph) {
            for (var loc : bb.locs) {
                for (var dstTemp : loc.instr.dsts) {
                    addNodeIfAbsent(dstTemp);
                    for (var liveTemp : loc.liveOut) {
                        addNodeIfAbsent(liveTemp);
                        regGraph.addEdgeUndirected(tempToNodeMap.get(liveTemp.index), tempToNodeMap.get(dstTemp.index));
                    }
                }
            }
        }
        regGraph.coloring(emitter.allocatableRegs.length);

        // allocate register for each basic block
        for (var bb : graph) {
            bb.label.ifPresent(subEmitter::emitLabel);
            localAlloc(bb, subEmitter);
        }

        subEmitter.emitEnd();
    }

    private void addNodeIfAbsent(Temp temp) {
        var allocatableRegList = new ArrayList<>(Arrays.asList(emitter.allocatableRegs));

        if (!tempToNodeMap.containsKey(temp.index)) {
            tempToNodeMap.put(temp.index, regGraph.nodes.size());
            regGraph.addNode();
            if (temp instanceof Reg) {
                regGraph.nodes.get(regGraph.nodes.size() - 1).color = allocatableRegList.indexOf(temp);
            }
        }
    }

    private void localAlloc(BasicBlock<PseudoInstr> bb, SubroutineEmitter subEmitter) {

        var callerNeedSave = new ArrayList<Reg>();

        for (var loc : bb.locs) {
            // Handle special instructions on caller save/restore.

            if (loc.instr instanceof HoleInstr) {
                if (loc.instr.equals(HoleInstr.CallerSave)) {
                    for (var reg : emitter.callerSaveRegs) {
                        if (reg.occupied && loc.liveOut.contains(reg.temp)) {
                            callerNeedSave.add(reg);
                            subEmitter.emitStoreToStack(reg);
                        }
                    }
                    continue;
                }

                if (loc.instr.equals(HoleInstr.CallerRestore)) {
                    for (var reg : callerNeedSave) {
                        subEmitter.emitLoadFromStack(reg, reg.temp);
                    }
                    callerNeedSave.clear();
                    continue;
                }
            }

            // For normal instructions: allocate registers for every read/written temp. Skip the already specified
            // special registers.
            allocForLoc(loc, subEmitter);
        }
    }

    private void allocForLoc(Loc<PseudoInstr> loc, SubroutineEmitter subEmitter) {
        var instr = loc.instr;
        var srcRegs = new Reg[instr.srcs.length];
        var dstRegs = new Reg[instr.dsts.length];

        for (var i = 0; i < instr.srcs.length; i++) {
            var temp = instr.srcs[i];
            if (temp instanceof Reg) {
                srcRegs[i] = (Reg) temp;
            } else {
                srcRegs[i] = allocRegFor(temp);
            }
        }

        for (var i = 0; i < instr.dsts.length; i++) {
            var temp = instr.dsts[i];
            if (temp instanceof Reg) {
                dstRegs[i] = ((Reg) temp);
            } else {
                dstRegs[i] = allocRegFor(temp);
            }
        }

        subEmitter.emitNative(instr.toNative(dstRegs, srcRegs));
    }

    private Reg allocRegFor(Temp temp) {

        if (tempToNodeMap.containsKey(temp.index)) {
            var nodeIndex = tempToNodeMap.get(temp.index);
            var allocRegIndex = regGraph.nodes.get(nodeIndex).color;
            var reg = emitter.allocatableRegs[allocRegIndex];

            reg.occupied = true;
            reg.temp = temp;
            return reg;
        }

        throw new Error("Sorry that spill is not implemented");
    }
}
