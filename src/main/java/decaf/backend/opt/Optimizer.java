package decaf.backend.opt;

import decaf.backend.dataflow.BasicBlock;
import decaf.backend.dataflow.CFG;
import decaf.backend.dataflow.CFGBuilder;
import decaf.backend.dataflow.Loc;
import decaf.driver.Config;
import decaf.driver.Phase;
import decaf.lowlevel.label.FuncLabel;
import decaf.lowlevel.tac.Simulator;
import decaf.lowlevel.tac.TacInstr;
import decaf.lowlevel.tac.TacProg;
import org.apache.commons.lang3.tuple.Pair;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.*;
import java.util.function.Function;

/**
 * TAC optimization phase: optimize a TAC program.
 * <p>
 * The original decaf compiler has NO optimization, thus, we implement the transformation as identity function.
 */
public class Optimizer extends Phase<TacProg, TacProg> {
    public Optimizer(Config config) {
        super("optimizer", config);
    }

    @Override
    public TacProg transform(TacProg input) {
        for (var func : input.funcs) {
            for (int numOpt = 0; numOpt < 2; numOpt++) {
                var graph = new CFGBuilder<TacInstr>().buildFrom(func.getInstrSeq());

                simplifyGraph(graph);

                new CopyPropagation().accept(graph);
                new ConstPropagation().accept(graph);
                new AlivenessAnalyzer().accept(graph);

                func.instrSeq = graph2instr(graph, func.entry);
            }
        }
        return input;
    }

    private List<TacInstr> graph2instr(CFG<TacInstr> graph, FuncLabel funcLabel) {
        var newInstrSeq = new ArrayList<TacInstr>();
        newInstrSeq.add(new TacInstr.Mark(funcLabel));

        for (var bb : graph.nodes) {
            if (bb.id == 0 || !graph.getPrev(bb.id).isEmpty()) {
                bb.label.ifPresent(label -> newInstrSeq.add(new TacInstr.Mark(label)));
                bb.locs.forEach((Loc<TacInstr> tacInstrLoc) -> {
                    newInstrSeq.add(tacInstrLoc.instr);
                });
            }
        }
        return newInstrSeq;
    }

    private void simplifyGraph(CFG<TacInstr> graph) {
        // bfs
        var visited = new boolean[graph.nodes.size()];
        Arrays.fill(visited, false);

        var queue = new LinkedList<Integer>();
        var root = 0;
        queue.offer(root);
        visited[root] = true;
        while (!queue.isEmpty()) {
            var node = queue.poll();
            for (var succ : graph.getSucc(node)) {
                if (!visited[succ]) {
                    queue.offer(succ);
                    visited[succ] = true;
                }
            }
        }

        // eliminate edges connected to unreachable nodes
        for (var node = visited.length - 1; node > -1; node--) {
            if (!visited[node]) {
                removeNode(graph, node);
            }
        }

        // eliminate redundant branches and labels
        for (var currIdx = graph.nodes.size() - 1; currIdx > 0; currIdx--) {
            var curr = graph.nodes.get(currIdx);
            var prev = graph.nodes.get(currIdx - 1);
            if (curr.label.isPresent()) {
                if (!prev.locs.isEmpty()) {
                    var instr = prev.locs.get(prev.locs.size() - 1).instr;
                    if (instr instanceof TacInstr.Branch && ((TacInstr.Branch) instr).target.compareTo(curr.label.get()) == 0) {
                        prev.locs.remove(prev.locs.size() - 1);
                        prev.kind = BasicBlock.Kind.CONTINUOUS;
                    }
                }
            }
        }
    }

    private void removeNode(CFG<TacInstr> graph, int node) {
        Function<Integer, Integer> renewIndex = (Integer oldNode) -> (oldNode > node) ? oldNode - 1 : oldNode;

        // remove edges and links
        graph.edges.removeIf((Pair<Integer, Integer> edge) ->
                edge.getKey() == node || edge.getValue() == node
        );
        for (var link : graph.links) {
            link.getKey().removeIf((Integer orphan) -> orphan == node);
            link.getValue().removeIf((Integer orphan) -> orphan == node);
        }
        // renew nodes behind
        for (var nodeIdx = node + 1; nodeIdx < graph.nodes.size(); nodeIdx++) {
            graph.nodes.get(nodeIdx).id -= 1;
        }
        for (var edgeIdx = 0; edgeIdx < graph.edges.size(); edgeIdx++) {
            var edge = graph.edges.get(edgeIdx);
            var newSrc = renewIndex.apply(edge.getKey());
            var newDst = renewIndex.apply(edge.getValue());
            graph.edges.set(edgeIdx, Pair.of(newSrc, newDst));
        }
        for (var linkIdx = 0; linkIdx < graph.links.size(); linkIdx++) {
            var link = graph.links.get(linkIdx);

            Set<Integer> lefts = new TreeSet<>();
            for (var left : link.getLeft()) {
                lefts.add(renewIndex.apply(left));
            }
            Set<Integer> rights = new TreeSet<>();
            for (var right : link.getRight()) {
                rights.add(renewIndex.apply(right));
            }
            graph.links.set(linkIdx, Pair.of(lefts, rights));
        }
        graph.nodes.remove(node);
        graph.links.remove(node);
    }

    @Override
    public void onSucceed(TacProg program) {
        if (config.target.equals(Config.Target.PA4)) {
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
