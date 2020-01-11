package decaf.backend.opt;

import decaf.backend.dataflow.BasicBlock;
import decaf.backend.dataflow.CFG;
import decaf.backend.dataflow.Loc;
import decaf.lowlevel.instr.Temp;
import decaf.lowlevel.tac.TacInstr;
import org.apache.commons.lang3.tuple.Pair;

import java.util.*;
import java.util.function.Consumer;


public class ConstPropagation implements Consumer<CFG<TacInstr>> {

    private Set<Object> meetEntry(Set<Object> entriesA, Set<Object> entriesB) {
        Set<Object> resultEntries = new TreeSet<>();
        for (var entryObjA : entriesA) {
            var entryA = (Pair<Temp, ConstRhs>) entryObjA;
            Optional<ConstRhs> entryFound = Optional.empty();
            for (var entryObjB : entriesB) {
                var entryB = (Pair<Temp, ConstRhs>) entryObjB;
                if (entryA.getKey() == entryB.getKey()) {
                    entryFound = Optional.of(entryB.getValue());
                    break;
                }
            }
            var valueA = entryA.getValue();
            var valueB = entryFound.orElse(ConstRhs.UNDEF);
            var valueResult = ConstRhs.NAC;

            if (valueA.isConst() && valueB.isConst() && valueA.data == valueB.data) {
                valueResult = valueA;
            } else if (valueA.isUndef() && valueB.isUndef()) {
                valueResult = ConstRhs.UNDEF;
            } else if (valueA.isConst() && valueB.isUndef()) {
                valueResult = valueA;
            } else if (valueA.isUndef() && valueB.isConst()) {
                valueResult = valueB;
            }
            resultEntries.add(Pair.of(entryA.getKey(), valueResult));
        }
        return resultEntries;
    }

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
                // meet function
                var prevs = new ArrayList<>(graph.getPrev(bb.id));
                Set<Object> newIn = new TreeSet<>();
                if (!prevs.isEmpty()) {
                    newIn = graph.getBlock(prevs.get(0)).out;
                }
                for (var prevIdx = 1; prevIdx < prevs.size(); prevIdx++) {
                    newIn = meetEntry(newIn, graph.getBlock(prevs.get(prevIdx)).out);
                }

                if (!newIn.equals(bb.in)) {
                    changed = true;
                    // propagation function
                    bb.in = newIn;
                    bb.out = new TreeSet<>();
                    bb.out.addAll(bb.in);
                    bb.out.removeIf((Object object) -> bb.kill.contains(((Pair<Temp, ConstRhs>) object).getKey()));
                    bb.out.addAll(bb.gen);
                }
            }
        }

        // do optimization
        for (var bb : graph.nodes) {
            var constPropMap = new HashMap<Temp, ConstRhs>();
            for (var constPropObject : bb.in) {
                var constPropEntry = (Pair<Temp, ConstRhs>) (constPropObject);
                constPropMap.put(constPropEntry.getKey(), constPropEntry.getValue());
            }

            var newLocs = new ArrayList<>(bb.locs);
            for (var loc : bb.locs) {
                // update copy prop map
                var ctx = new ConstPropVisitor(constPropMap);
                loc.instr.accept(ctx);

                var idx = newLocs.indexOf(loc);
                if (idx > -1) {
                    if (ctx.isDropped) {
                        newLocs.remove(idx);
                    } else {
                        ctx.replacement.ifPresent(replacement -> newLocs.set(idx, new Loc<>(replacement)));
                    }
                }
            }
            bb.locs = newLocs;
        }
    }

    private void computeGenAndKillFor(BasicBlock<TacInstr> bb) {
        bb.gen = new TreeSet<>();
        bb.kill = new TreeSet<>();

        var constPropMap = new HashMap<Temp, ConstRhs>();
        for (var loc : bb) {
            bb.kill.addAll(loc.instr.getWritten());

            loc.instr.accept(new ConstPropVisitor(constPropMap));
        }
        for (var entry : constPropMap.entrySet()) {
            bb.gen.add(Pair.of(entry.getKey(), entry.getValue()));
        }
    }

    static class ConstPropVisitor implements TacInstr.Visitor {
        Optional<TacInstr> replacement = Optional.empty();

        boolean isDropped = false;

        HashMap<Temp, ConstRhs> constRhsHashMap;

        ConstPropVisitor(HashMap<Temp, ConstRhs> constRhsHashMap) {
            this.constRhsHashMap = constRhsHashMap;
        }

        @Override
        public void visitAssign(TacInstr.Assign instr) {
            var dstValue = ConstRhs.NAC;
            if (constRhsHashMap.containsKey(instr.src)) {
                dstValue = constRhsHashMap.get(instr.src);
                if (dstValue.isConst()) {
                    replacement = Optional.of(new TacInstr.LoadImm4(instr.dst, dstValue.data));
                }
            }
            constRhsHashMap.put(instr.dst, dstValue);
        }

        @Override
        public void visitLoadVTbl(TacInstr.LoadVTbl instr) {
            constRhsHashMap.put(instr.dst, ConstRhs.UNDEF);
        }

        @Override
        public void visitLoadImm4(TacInstr.LoadImm4 instr) {
            constRhsHashMap.put(instr.dst, ConstRhs.of(instr.value));
        }

        @Override
        public void visitLoadStrConst(TacInstr.LoadStrConst instr) {
            constRhsHashMap.put(instr.dst, ConstRhs.UNDEF);
        }

        @Override
        public void visitUnary(TacInstr.Unary instr) {
            var dstValue = ConstRhs.NAC;
            if (constRhsHashMap.containsKey(instr.operand)) {
                var operandValue = constRhsHashMap.get(instr.operand);

                if (operandValue.isConst()) {
                    var operand = operandValue.data;
                    int result = switch (instr.op) {
                        case NEG -> -operand;
                        case LNOT -> (operand == 0) ? 1 : 0;
                    };
                    dstValue = ConstRhs.of(result);
                } else {
                    dstValue = operandValue;
                }
            }
            constRhsHashMap.put(instr.dst, dstValue);
        }

        @Override
        public void visitBinary(TacInstr.Binary instr) {
            var dstValue = ConstRhs.NAC;
            if (constRhsHashMap.containsKey(instr.lhs) && constRhsHashMap.containsKey(instr.rhs)) {
                var lhsValue = constRhsHashMap.get(instr.lhs);
                var rhsValue = constRhsHashMap.get(instr.rhs);
                if (lhsValue.isConst() && rhsValue.isConst()) {
                    var lhs = lhsValue.data;
                    var rhs = rhsValue.data;
                    int result = switch (instr.op) {
                        case ADD -> lhs + rhs;
                        case SUB -> lhs - rhs;
                        case MUL -> lhs * rhs;
                        case DIV -> (rhs == 0) ? 0 : lhs / rhs;
                        case MOD -> (rhs == 0) ? 0 : lhs % rhs;
                        case EQU -> (lhs == rhs) ? 1 : 0;
                        case NEQ -> (lhs != rhs) ? 1 : 0;
                        case LES -> (lhs < rhs) ? 1 : 0;
                        case LEQ -> (lhs <= rhs) ? 1 : 0;
                        case GTR -> (lhs > rhs) ? 1 : 0;
                        case GEQ -> (lhs >= rhs) ? 1 : 0;
                        case LAND -> (lhs != 0 && rhs != 0) ? 1 : 0;
                        case LOR -> (lhs != 0 || rhs != 0) ? 1 : 0;
                    };
                    dstValue = ConstRhs.of(result);
                    replacement = Optional.of(new TacInstr.LoadImm4(instr.dst, result));
                } else if (lhsValue.isNac() || rhsValue.isNac()) {
                    dstValue = ConstRhs.NAC;
                } else {
                    dstValue = ConstRhs.UNDEF;
                }
            }
            constRhsHashMap.put(instr.dst, dstValue);
        }

        @Override
        public void visitCondBranch(TacInstr.CondBranch instr) {
            if (constRhsHashMap.containsKey(instr.cond)) {
                var value = constRhsHashMap.get(instr.cond);
                if (value.isConst()) {
                    if (value.data == 0) {
                        replacement = Optional.of(new TacInstr.Branch(instr.target));
                    } else {
                        isDropped = true;
                    }
                }
            }
        }

        @Override
        public void visitIndirectCall(TacInstr.IndirectCall instr) {
            instr.dst.ifPresent(dst -> constRhsHashMap.put(dst, ConstRhs.NAC));
        }

        @Override
        public void visitDirectCall(TacInstr.DirectCall instr) {
            instr.dst.ifPresent(dst -> constRhsHashMap.put(dst, ConstRhs.NAC));
        }

        @Override
        public void visitMemory(TacInstr.Memory instr) {
            switch (instr.op) {
                case LOAD -> {
                    constRhsHashMap.put(instr.dst, ConstRhs.NAC);
                }
                case STORE -> {
                }
            }
        }
    }
}

class ConstRhs {
    enum ValueType {
        CONST, NAC, UNDEF
    }

    int data;
    private ValueType type;

    static final ConstRhs UNDEF = new ConstRhs(ValueType.UNDEF, 0);
    static final ConstRhs NAC = new ConstRhs(ValueType.NAC, 0);

    static ConstRhs of(int data) {
        return new ConstRhs(ValueType.CONST, data);
    }

    ConstRhs(ValueType type, int data) {
        this.type = type;
        this.data = data;
    }

    boolean isNac() {
        return type == ValueType.NAC;
    }

    boolean isUndef() {
        return type == ValueType.UNDEF;
    }

    boolean isConst() {
        return type == ValueType.CONST;
    }

    @Override
    public String toString() {
        return switch (type) {
            case CONST -> "CONST " + data;
            case NAC -> "NAC";
            case UNDEF -> "UNDEF";
        };
    }
}