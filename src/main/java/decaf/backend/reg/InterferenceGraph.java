package decaf.backend.reg;

import java.util.ArrayList;
import java.util.Arrays;

class Node {
    int color = -1;
}

public class InterferenceGraph {

    public ArrayList<Node> nodes = new ArrayList<>();

    public ArrayList<ArrayList<Integer>> edges = new ArrayList<>();

    public InterferenceGraph() {
    }

    public void addNode() {
        nodes.add(new Node());
        edges.add(new ArrayList<>());
    }

    public boolean hasEdge(int src, int dst) {
        return edges.get(src).indexOf(dst) > -1;
    }

    public void addEdgeDirected(int src, int dst) {
        if (!hasEdge(src, dst)) {
            edges.get(src).add(dst);
        }
    }

    public void addEdgeUndirected(int src, int dst) {
        addEdgeDirected(src, dst);
        addEdgeDirected(dst, src);
    }

    public void coloring(int maxColorNum) {
        var availableColors = new boolean[maxColorNum];

        if (nodes.isEmpty()) {
            return;
        }

        for (var node = 0; node < nodes.size(); node++) {
            if (nodes.get(node).color > -1) {
                // already allocated
                continue;
            }
            Arrays.fill(availableColors, true);
            for (var neighbour : edges.get(node)) {
                if (nodes.get(neighbour).color > -1) {
                    availableColors[nodes.get(neighbour).color] = false;
                }
            }
            for (var color = 0; color < availableColors.length; color++) {
                if (availableColors[color]) {
                    nodes.get(node).color = color;
                    break;
                }
            }
            if (nodes.get(node).color < 0) {
                throw new Error("Coloring failed");
            }
        }
    }
}
