package org.encalmo

import org.encalmo.graph.Graph.GraphDfsVisitor

package object graph {

    implicit class GraphOps[N](val graph:Graph[N]) extends AnyVal {

        /** Set of leaf nodes (without edges beginning at)*/
        def leaves: Traversable[N] = Graph.leavesOf(graph)

        /** Set of root nodes (without edges leading to) */
        def roots: Traversable[N] = Graph.rootsOf(graph)

        /** Deep mutable copy of the graph */
        def deepCopy(): MutableMapGraph[N] = Graph.deepCopy(graph)

        /** Depth-first search of the whole graph */
        def dfs(visitor: GraphDfsVisitor[N]):Unit = Graph.dfs(graph, visitor)

        /** Depth-first search of the graph in the given order */
        def dfs(visitor: GraphDfsVisitor[N], nodes:Traversable[N]):Unit = Graph.dfs(graph,visitor,nodes)

        /** Depth-first search (recursive) of the graph starting at given node */
        def dfs(node:N, visitor: GraphDfsVisitor[N]):Unit = Graph.dfs(graph,node,visitor)

        /** Depth-first search (iterative) of the graph starting at given node */
        def dfsi(source:N, visitor: GraphDfsVisitor[N]):Unit = Graph.dfsi(graph,source,visitor)

        /** Breath-first search of the whole graph */
        def bfs(visitor: N => Unit):Unit = Graph.bfs(graph,visitor)

        /** Breath-first search of the graph starting at given node */
        def bfs(node: N, visitor: N => Unit):Unit = Graph.bfs(graph,node,visitor)

        /** Finds all cycles */
        def findCycles(): Vector[N] = Graph.findCycles(graph)

        /** Finds cycles starting at the given node */
        def findCycles(node: N): Vector[N] = Graph.findCycles(graph,node)

        /** Checks if graph contains cycles */
        def hasCycles(): Boolean = Graph.hasCycles(graph)

        /** Sorts nodes topologically */
        def sortTopologically(): List[N] = Graph.sortTopologically(graph)

        /** Dijkstra algorithm finds shortest path between two nodes */
        def findShortestPath[V:Numeric](from: N, to: N, weight: (N,N) => V): (V,List[(N,N)]) = Graph.findShortestPath(graph,from,to,weight)

        /** Dijkstra algorithm finds all shortest distances of paths starting at the given node*/
        def findShortestDistances[V:Numeric](from: N, weight: (N,N) => V): scala.collection.Map[N,V] = Graph.findShortestDistances(graph,from,weight)

        /* Kosaraju's 2-dfs pass algorithm finds strongly connected components */
        def findStronglyConnectedComponents(): Traversable[Traversable[N]] = Graph.findStronglyConnectedComponents(graph)

        /** Merge two nodes into one (collapse) */
        def mergeNodes(mergedNode: N, removedNode: N): MutableMapGraph[N] = Graph.mergeNodes(graph,mergedNode,removedNode)

        /** Filters out every one node that does not belong to one of the paths leading to one of the given target nodes **/
        def filterOutPathsNotLeadingTo(nodes: Traversable[N]): Graph[N] = Graph.filterOutPathsNotLeadingTo(graph,nodes)

    }

}
