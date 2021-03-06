package org.encalmo.graph

import org.scalatest.FunSpec
import scala.Predef._
import scalax.file.Path

class GraphTest extends FunSpec {

    describe("Graph"){

        val graph1 = Graph[Int](
            1 -> Seq(2,3),
            2 -> Seq(3),
            3 -> Seq(4),
            4 -> Seq()
        )

	    /* cyclic, connected components */
        val graph2 = Graph[Int](
            1 -> Seq(2,3,6),
            2 -> Seq(3),
            3 -> Seq(4),
            4 -> Seq(5,4),
            5 -> Seq(1,3),
            6 -> Seq(7,10),
            7 -> Seq(8),
            8 -> Seq(9,10),
            9 -> Seq(6),
            10 -> Seq(6,7),
            11 -> Seq(12,7),
            12 -> Seq(13),
            13 -> Seq(11)
        )

	    /* acyclic, weighted */
	    val graph3 = Graph[Int,Int](
		    1 -> Seq((2,1),(3,2),(4,3)),
		    2 -> Seq((3,3),(5,4)),
		    3 -> Seq((4,2),(5,2)),
		    4 -> Seq((5,3)),
		    5 -> Seq()
	    )

	    /* acyclic */
	    val graph4 = Graph[Int](
		    3 -> Seq(6),
		    4 -> Seq(2),
		    5 -> Seq(2,3,4,6),
		    6 -> Seq(),
		    2 -> Seq(3,6)
	    )

	    val graph5 = Graph[Int](
		    0 -> Seq(1,3),
		    1 -> Seq(0,2,3),
		    2 -> Seq(1,3),
		    3 -> Seq(0,1,2)
	    )

	    val graph6 = Graph[Int](
		    0 -> Seq(1,2,3),
		    1 -> Seq(0,2,3),
		    2 -> Seq(0,1,3),
		    3 -> Seq(0,1,2)
	    )

	    lazy val veryLargeGraph = Graph.readFromEdgeListFile(Path.fromString("src/main/resources/SCC.txt"))
	    lazy val weightedGraph = Graph.readFromAdjacentWeightListFile(Path.fromString("src/main/resources/dijkstraData.txt"))
	    lazy val minCutGraph = Graph.readFromAdjacentListFile(Path.fromString("src/main/resources/graph1.txt"))

        it("should have nodes and edges") {
            assert(graph1.nodes.size==4, "graph nodes count should be 4")
            assert(graph1.edges.size==4, "graph edges count should be 4")
        }
        it("should got reversed graph") {
            val reverse = graph1.reverse
            val redges = reverse.edges.toSeq
            val reversed2 = reverse.reverse
            val radjacentOf1 = reverse.adjacent(1).toSeq
            val radjacentOf4 = reverse.adjacent(4).toSeq
            assert(reverse.nodes.size == 4, "reversed graph nodes count should be 4")
            assert(redges.size == 4, "reversed graph edges count should be 4")
            assert(reversed2 == graph1, "twice reversed graph should be the same")
            assert(radjacentOf1.isEmpty)
            assert(radjacentOf4 == Seq(3))
        }
        it("should check graphs are similar"){
           val graph2 = weightedGraph.deepCopy()
           assert(graph2.isSameAs(weightedGraph))
           assert(weightedGraph.isSameAs(graph2))
        }
        it("should check if 1st graph is subset of the 2nd"){
            val graph2 = weightedGraph.filterOutPathsNotLeadingTo(weightedGraph.nodes.drop(weightedGraph.nodesCount/2).take(5))
            assert(!graph2.isSameAs(weightedGraph))
            assert(graph2.isSubsetOf(weightedGraph))
            assert(!weightedGraph.isSubsetOf(graph2))
        }
        it("should search graph with dfs") {
            val graph = graph2
            var counter = 0
            Graph.dfs(graph,new Graph.GraphDfsVisitor[Int] {
                override def before(node:Int) {
                    counter = counter + 1
                }
            })
            assert(counter==graph.nodesCount,s"counter should be ${graph.nodesCount} but is $counter")
        }
        it("should find strongly connected components") {
            val graph = graph2
            val result = Graph.findStronglyConnectedComponents(graph)
            assert(result.size==3,s"should be 3 but is ${result.size}")
        }
	    it("should read adjacent list graph from file") {
		    assert(minCutGraph.nodesCount==200)
		    assert(minCutGraph.adjacent(82).size==27)
	    }
	    it("should read adjacent-weight list graph from file") {
		    assert(weightedGraph.nodesCount==200)
		    assert(weightedGraph.weight(200,108)==9976)
		    assert(weightedGraph.adjacent(31).size==21)
	    }
	    it("should breath-first search the graph") {
		    var counter = 0
		    Graph.bfs(weightedGraph,{n:Int => counter = counter + 1})
		    assert(counter==weightedGraph.nodesCount,s"should be ${weightedGraph.nodesCount} but is ${counter}")
	    }
        it("should depth-first search the graph - dijkstraGraph") {
            var counter = 0
            Graph.dfs(weightedGraph,new Graph.GraphDfsVisitor[Int] {
                override def before(node:Int) {
                    counter = counter + 1
                }
            })
            assert(counter==weightedGraph.nodesCount,s"should be ${weightedGraph.nodesCount} but is ${counter}")
        }
	    it("should depth-first search the graph") {
		    var counter = 0
		    Graph.dfs(weightedGraph,new Graph.GraphDfsVisitor[Int] {
			    override def before(node:Int) {
				    counter = counter + 1
			    }
		    })
		    assert(counter==weightedGraph.nodesCount,s"should be ${weightedGraph.nodesCount} but is ${counter}")
	    }
	    it("should find cycles - graph2") {
		    val cycles = Graph.findCycles(graph2)
		    assert(cycles.size == 6)
	    }
	    it("should find cycles - graph3") {
		    val cycles = Graph.findCycles(graph3)
		    assert(cycles.isEmpty)
	    }
	    it("should check cycles") {
		    assert(Graph.hasCycles(graph2))
		    assert(!Graph.hasCycles(graph3))
	    }
	    it("should sort topologically - graph3") {
		    val order = Graph.sortTopologically(graph3)
		    assert(order.sameElements(Seq(1, 2, 3, 4, 5)))
	    }
	    it("should sort topologically - graph4") {
		    val order = Graph.sortTopologically(graph4)
		    assert(order.sameElements(Seq(5, 4, 2, 3, 6)),s"wrong order $order")
	    }
	    it("should compute shortest path - graph3") {
		    val (distance,path) = Graph.findShortestPath(graph3,1,5)
		    assert(distance==4,s"should be 4 but is $distance : $path")
		    assert(path===List((1,3), (3,5)),s"$path")
	    }
	    it("should compute shortest path - dijkstraData") {
		    assert(weightedGraph.nodesCount==200)
		    assert(weightedGraph.weight(200,108)==9976)
		    assert(weightedGraph.adjacent(31).size==21)
		    val path1 = Graph.findShortestPath(weightedGraph,1,197)
		    assert(path1===(3068,List((1,114), (114,103), (103,110), (110,197))))
		    val path2 = Graph.findShortestPath(weightedGraph,1,115)
		    assert(path2===(2399,List((1,80), (80,115))),s"$path2")
	    }
	    it("should compute all shortest paths - graph3") {
		    val distance = Graph.findShortestPaths(graph3,1)
		    assert(distance.size==5)
		    assert(distance===Map(1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 4))
	    }
	    it("should compute all shortest paths - dijkstraData") {
		    assert(weightedGraph.nodesCount==200)
		    assert(weightedGraph.weight(200,108)==9976)
		    assert(weightedGraph.adjacent(31).size==21)
		    val distance = Graph.findShortestPaths(weightedGraph,1)
		    val nodes = Seq(7,37,59,82,99,115,133,165,188,197)
		    val result = nodes map distance
		    assert(result===List(2599, 2610, 2947, 2052, 2367, 2399, 2029, 2442, 2505, 3068))
	    }
	    it("should merge nodes") {
		    val g1 = Graph.mergeNodes(graph5,1,0)
		    assert(!g1.contains(0))
		    assert(g1.contains(1))
		    val g2 = Graph.mergeNodes(graph6,2,1)
		    assert(!g2.contains(1))
		    assert(g2.contains(2))
	    }
	    it("should find min cut count") {
		    var count = Integer.MAX_VALUE
		    for(i <- 1 to 27){
			    count = Math.min(Graph.randomCutCount(minCutGraph),count)
		    }
		    assert(count==17)
	    }
	    ignore("should find strongly connected components - scc") {
			val result = Graph.findStronglyConnectedComponents(veryLargeGraph)
		    val ten: Seq[Int] = (result.take(10) map (_.size)).toSeq
			assert(ten.sameElements(Seq(434821, 968, 459, 313, 211, 205, 197, 177, 162, 152)))
		}
        it("should remove single node from mutable graph") {
            val graph = Graph.deepCopy(graph1)
            assert(graph.contains(3))
            graph.remove(3)
            assert(!graph.contains(3))
            val reverse = graph.reverse
            assert(!reverse.contains(3))
            assert(graph.contains(1))
            assert(graph.contains(4))
            assert(graph.contains(2))
        }
        it("should remove set of nodes from mutable graph") {
            val graph = Graph.deepCopy(graph1)
            assert(graph.contains(3))
            assert(graph.contains(2))
            graph.remove(List(3,2))
            assert(!graph.contains(3))
            assert(!graph.contains(2))
            val reverse = graph.reverse
            assert(!reverse.contains(3))
            assert(!reverse.contains(2))
            assert(graph.contains(1))
            assert(graph.contains(4))
        }
        it("should return leaves of graph") {
            val leaves1 = Graph.leavesOf(graph1)
            assert(leaves1.toList == List(4))
            val leaves2 = Graph.leavesOf(graph2)
            val list2: List[Int] = leaves2.toList
            assert(list2 == Nil)
            val leaves3 = Graph.leavesOf(graph3)
            val list3: List[Int] = leaves3.toList
            assert(list3 == List(5))
            val leaves4 = Graph.leavesOf(graph4)
            val list4: List[Int] = leaves4.toList
            assert(list4 == List(6))
        }
        it("should return roots of graph") {
            val roots1 = Graph.rootsOf(graph1)
            assert(roots1.toList == List(1))
            val roots2 = Graph.rootsOf(graph2)
            val list2: List[Int] = roots2.toList
            assert(list2 == Nil)
            val roots3 = Graph.rootsOf(graph3)
            val list3: List[Int] = roots3.toList
            assert(list3 == List(1))
            val roots4 = Graph.rootsOf(graph4)
            val list4: List[Int] = roots4.toList
            assert(list4 == List(5))
        }
        it("should map graph") {
            val mapped1 = graph1.map(n => n + 10)
            assert(mapped1.adjacent(11).toList == List(12,13))
            assert(mapped1.nodesCount == 4)
            assert(mapped1.edgesCount == graph1.edgesCount)
            val mapped2 = graph1.map(n => (n+1)/2)
            assert(mapped2.adjacent(1).toList == List(1,2,2))
            assert(mapped2.nodesCount == 2)
            assert(mapped2.edgesCount == graph1.edgesCount)
            val chars = "ABCDEFGHIJKLMOPQRSTUVWXYZ"
            val mapped3 = graph1.map(n => chars(n))
            assert(mapped3.adjacent('B').toList == List('C','D'))
            assert(mapped3.nodesCount == 4)
            assert(mapped3.edgesCount == graph1.edgesCount)
        }
        it("should filter out nodes not on paths leading to the given target nodes"){
            val graph = Graph[Int](
                1 -> Seq(2,5),
                2 -> Seq(3),
                3 -> Seq(4),
                4 -> Seq(6,7),
                5 -> Seq(8),
                6 -> Seq(8,9),
                7 -> Seq(10),
                8 -> Seq(),
                9 -> Seq(),
               10 -> Seq()
            )
            val graph2:Graph[Int] = graph.filterOutPathsNotLeadingTo(List(10))
            assert(graph2.adjacent(1).toSeq == Seq(2))
            assert(graph2.adjacent(4).toSeq == Seq(7))
        }
    }

}
