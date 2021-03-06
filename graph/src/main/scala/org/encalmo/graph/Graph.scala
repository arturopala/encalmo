package org.encalmo.graph

import scala.specialized
import scalax.file.Path
import scala.collection.mutable

trait Graph[@specialized(Int) N] {
    
    def nodes: Traversable[N]
    def adjacent: N => Traversable[N]
    def edges: Traversable[(N,N)]
	def contains(node: N): Boolean
    def reverse: Graph[N]
	def nodesCount:Int
	def edgesCount:Long

    def filter(f: N => Boolean): Graph[N]
    def foreach[U](f: (N) => U):Unit
    def map[U](f: (N) => U): Graph[U]
}

trait Unidirected[@specialized(Int) N] extends Graph[N] {
	override val reverse: Graph[N] = this
}

trait Weighted[@specialized(Int) N, @specialized(Int,Double) V] {
    def weight: (N,N) => V
}

trait GenericGraph[@specialized(Int) N] extends Graph[N] {
	self =>
    override def edges: Traversable[(N,N)] = new Traversable[(N, N)] {
	    override def foreach[U](f: ((N, N)) => U):Unit = for(from <- nodes; to <- adjacent(from)) f((from,to))
    }
	override def contains(node: N): Boolean = nodes match {
		case s:Set[N] => s.contains(node)
		case _ => nodes exists (n => n==node)
	}
    override def reverse:Graph[N] = new GenericReversedGraph[N](self)
    override def nodesCount: Int = nodes.size
    override def edgesCount: Long = nodes.foldLeft(0L){case (sum,node) => sum + adjacent(node).size}

    override def filter(f: N => Boolean): Graph[N] = new FilteredGraph[N](self,f)
    override def foreach[U](f: (N) => U):Unit = nodes.foreach(f)
    override def map[U](f: (N) => U): Graph[U] = new GenericGraph[U] {
        def nodes: Traversable[U] = self.nodes.map(f)
        val adjacent: (U) => Traversable[U] = {
            node => self.nodes.filter(f(_)==node).flatMap(self.adjacent).map(f)
        }
    }
}

class GenericReversedGraph[@specialized(Int) N](origin: Graph[N]) extends GenericGraph[N] {
    override def nodes: Traversable[N] = origin.nodes
    override val adjacent: N => Traversable[N] = node => new Traversable[N]{
        def foreach[U](f: (N) => U) {
            for (n <- origin.nodes if (origin.adjacent(n) match {
                case set:Set[N] => set contains node
                case col => col exists (_ == node)
            })) f(n)
        }
    }
    override def edges: Traversable[(N,N)] =  new Traversable[(N, N)] {
	    def foreach[U](f: ((N, N)) => U) {
		    for(from <- origin.nodes; to <- origin.adjacent(from)) f((to,from))
	    }
    }
    override val reverse = origin
}

class MapGraph[@specialized(Int) N](val nodeMap: Map[N,Traversable[N]] = Map[N,Traversable[N]]()) extends GenericGraph[N]{
	override val nodes: Iterable[N] =  nodeMap.keys
	override val adjacent: N => Traversable[N] = nodeMap

	override lazy val reverse: Graph[N] = {
        val self = this
        new MutableMapGraph[N](){
            override lazy val reverse: Graph[N] = self
        }.add(this.nodes).addEdgesReversed(this.edges)
    }

    override def nodesCount: Int = nodeMap.size
	override def contains(node: N): Boolean = nodeMap.contains(node)
    override def map[U](f: (N) => U): MapGraph[U] = {
        val newNodeMap = mutable.Map[U,mutable.ArrayBuffer[U]]()
        nodeMap.foreach {
            case (node,adj) => {
                val newNode = f(node)
                val newAdjacent = adj.map(f)
                newNodeMap.getOrElseUpdate(newNode,{new mutable.ArrayBuffer[U]()}) ++= newAdjacent
            }
        }
        new MapGraph[U](Map() ++ newNodeMap)
    }
}

trait MutableGraph[@specialized(Int) N] extends GenericGraph[N] {
    
    def add(node: N): this.type
    def add(nodes: Traversable[N]): this.type

    def remove(node:N): this.type
    def remove(nodes: Traversable[N]): this.type
    
    def link(edge: (N,N), allowDuplicates: Boolean): this.type
    def link(from: N, to: N, allowDuplicates: Boolean): this.type
    def link(edges: Traversable[(N,N)], allowDuplicates: Boolean): this.type

    def unlink(edge: (N,N)): this.type
    def unlink(from: N, to: N): this.type
    def unlink(edges: Traversable[(N,N)]): this.type

    def clear(): Unit
}

class MutableMapGraph[@specialized(Int) N](
    val nodeMap: mutable.Map[N,mutable.ArrayBuffer[N]] = new mutable.LinkedHashMap[N,mutable.ArrayBuffer[N]]()
) extends MutableGraph[N] {
    self =>
    override def nodes:Iterable[N] =  nodeMap.keys
    override val adjacent: N => mutable.ArrayBuffer[N] = nodeMap

	override def reverse: Graph[N] = {
        new MutableMapGraph[N](){
            override lazy val reverse: Graph[N] = self
        }.add(this.nodes).addEdgesReversed(this.edges)
    }

	override def nodesCount: Int = nodeMap.size
	override def contains(node: N): Boolean = nodeMap.contains(node)

    override def map[U](f: (N) => U): MutableMapGraph[U] = {
        val newNodeMap = mutable.Map[U,mutable.ArrayBuffer[U]]()
        nodeMap.foreach {
            case (node, adj) => {
                val newNode = f(node)
                val newAdjacent = adj.map(f)
                newNodeMap.getOrElseUpdate(newNode,{new mutable.ArrayBuffer[U]()}) ++= newAdjacent
            }
        }
        new MutableMapGraph[U](newNodeMap)
    }

    override def add(node: N): this.type = {
        nodeMap.getOrElseUpdate(node,{new mutable.ArrayBuffer[N]()})
        this
    }

    override def remove(node:N): this.type = {
        nodeMap.remove(node)
        for(adjacent <- nodeMap.values){
            adjacent -= node
        }
        this
    }

    override def add(nodes: Traversable[N]): this.type = {
        nodes.foreach(this.add)
        this
    }

    override def remove(nodes: Traversable[N]): this.type = {
        nodes.foreach(nodeMap.remove)
        for(adjacent <- nodeMap.values){
            adjacent --= nodes
        }
        this
    }

    override def link(edge: (N,N), allowDuplicates: Boolean): this.type = link(edge._1, edge._2, allowDuplicates)

    override def link(from: N, to: N, allowDuplicates: Boolean): this.type = {
        val adj = nodeMap.getOrElseUpdate(from,{new mutable.ArrayBuffer[N]()})
        if (allowDuplicates || !adj.contains(to)) adj += to
        nodeMap.getOrElseUpdate(to,{new mutable.ArrayBuffer[N]()})
        this
    }

    override def link(edges: Traversable[(N,N)], allowDuplicates: Boolean): this.type = {
        edges.foreach(edge => {
            this.link(edge,allowDuplicates)
        })
        this
    }

	override def unlink(edge: (N,N)): this.type = {
		for(adjacent <- nodeMap.get(edge._1)){
			adjacent -= edge._2
		}
		this
	}

    override def unlink(from: N, to: N): this.type = {
        for(adjacent <- nodeMap.get(from)){
            adjacent -= to
        }
        this
    }


    override def unlink(edges: Traversable[(N,N)]): this.type = {
        edges.foreach(edge => {
            this.unlink(edge)
        })
        this
    }

    def addEdgesReversed(edges:Traversable[(N,N)]): this.type = {
        edges.foreach(edge => {
            this.link(edge.swap,true)
        })
        this
    }

	override def clear(): Unit = {
        nodeMap.clear()
    }
}

class FilteredGraph[N](origin:Graph[N], funct: N => Boolean) extends GenericGraph[N] {
    self =>
    override val nodes: Traversable[N] = origin.nodes.filter(funct)
    override val adjacent: (N) => Traversable[N] = origin.adjacent.andThen(_.filter(funct))
    override def edges: Traversable[(N, N)] = new Traversable[(N, N)] {
        override def foreach[U](f: ((N, N)) => U):Unit = for(from <- origin.nodes.withFilter(funct); to <- origin.adjacent(from).withFilter(funct)) f((from,to))
    }
    override def contains(node: N): Boolean = if(funct(node)) origin.contains(node) else false
    override def reverse: Graph[N] = new FilteredGraph[N](origin.reverse,funct)

    override def filter(f: N => Boolean): Graph[N] = new FilteredGraph[N](origin, {n:N => funct(n) && f(n)})
    override def foreach[U](f: (N) => U):Unit = origin.nodes.withFilter(funct).foreach(f)
    override def map[U](f: (N) => U): Graph[U] = new GenericGraph[U] {
        def nodes: Traversable[U] = origin.nodes.withFilter(funct).map(f)
        val adjacent: (U) => Traversable[U] = {
            node => origin.nodes.filter(funct).filter(f(_)==node).flatMap(self.adjacent).map(f)
        }
    }
}

object Graph {

    class GenericGraphImpl[@specialized(Int) N](val nodes:Iterable[N], val adjacent: N => Traversable[N]) extends GenericGraph[N]
	class WeightedGraphImpl[@specialized(Int) N, @specialized(Double,Int) V:Numeric](val nodes:Iterable[N], val adjacent: N => Traversable[N], val weight: (N,N) => V) extends GenericGraph[N] with Weighted[N,V]

	def apply[@specialized(Int) N](): MutableGraph[N] = new MutableMapGraph[N]()
	def apply[@specialized(Int) N](map: Map[N,Traversable[N]]): Graph[N] = new MapGraph(map)
    def apply[@specialized(Int) N](mappings:(N,Traversable[N])*): Graph[N] = new MapGraph(mappings.toMap)
	def apply[@specialized(Int) N](nodes:Iterable[N], adjacent: N => Traversable[N]): Graph[N] = new GenericGraphImpl[N](nodes,adjacent)
    def apply[@specialized(Int) N](edges:Traversable[(N,N)]): MutableGraph[N] = new MutableMapGraph[N]() link (edges,true)
	def apply[@specialized(Int) N, @specialized(Double,Int) V:Numeric](mappings:(N,Iterable[(N,V)])*): Graph[N] with Weighted[N,V] = {
		val nodeWeightMap = mappings.toMap map {case (k,v) => (k,v.toMap)}
		new WeightedGraphImpl[N,V](nodeWeightMap.keys, nodeWeightMap.mapValues{case m => m.keys}, (t:N,h:N) => nodeWeightMap(t)(h))
	}
	

	def readFromEdgeListFile(path:Path, reversed:Boolean = false):Graph[Int] = {
        val edges = path.lines().view map (line => {
            val i = line.indexOf(' ')
            val tail = line.substring(0,i).toInt
            val head = line.substring(i+1).trim.toInt
            if (reversed) (head,tail) else (tail,head)
        })
        Graph(edges)
	}

	def readFromAdjacentListFile(path:Path): Graph[Int] = {
		def parseNodeAdjacentList(line:String):(Int,Seq[Int]) = {
			val tokens = line.split('\t')
			if(tokens.length==0) return null
			val label:Int = Integer.parseInt(tokens(0))
			val adjacent:Seq[Int] = tokens.drop(1) map (_.toInt)
			(label,adjacent)
		}
		val nodeMap = mutable.LinkedHashMap[Int,Traversable[Int]]()
		for (line <-path.lines() if !line.trim.isEmpty) {
			val (node, adjacent) = parseNodeAdjacentList(line)
			nodeMap(node) = adjacent
		}
		new GenericGraphImpl[Int](nodeMap.keys, nodeMap)
	}

	def readFromAdjacentWeightListFile(path:Path): Graph[Int] with Weighted[Int,Int] = {
		def parseNodeWeightAdjacentList(line:String):(Int,Map[Int,Int]) = {
			val tokens = line.split('\t')
			if(tokens.length==0) return null
			val label:Int = Integer.parseInt(tokens(0))
			val adjacent:Map[Int,Int] = (tokens.drop(1) map parseNodeWeight).toMap
			(label,adjacent)
		}
		def parseNodeWeight(token:String): (Int,Int) = {
			val nw = token.split(',') map (_.toInt); (nw(0),nw(1))
		}
		val nodeWeightMap = mutable.LinkedHashMap[Int,Map[Int,Int]]()
		for (line <-path.lines() if !line.trim.isEmpty) {
			val (node, list) = parseNodeWeightAdjacentList(line)
			nodeWeightMap(node) = list
		}
		new WeightedGraphImpl[Int,Int](nodeWeightMap.keys, nodeWeightMap.mapValues{case m => m.keys}, (t:Int,h:Int) => nodeWeightMap(t)(h))
	}

    /** Checks if graphs contains the same nodes and edges */
    def isSame[@specialized(Int) N](g1:Graph[N],g2:Graph[N]):Boolean = {
        g1.nodesCount==g2.nodesCount && g1.edgesCount==g2.edgesCount && g1.nodes.forall(n =>
           g2.contains(n) && g1.adjacent(n).toSet.sameElements(g2.adjacent(n).toSet)
        )
    }

    /** Checks if 1st graph is subset of the 2nd  */
    def isSubset[@specialized(Int) N](g1:Graph[N],g2:Graph[N]):Boolean = {
        g1.nodesCount <= g2.nodesCount && g1.edgesCount <= g2.edgesCount && g1.nodes.forall(n => {
            g2.contains(n) && g1.adjacent(n).forall(g2.adjacent(n).toSet)
        })
    }

    /** Deep mutable copy of the graph */ 
	def deepCopy[@specialized(Int) N](graph:Graph[N]): MutableMapGraph[N] =  new MutableMapGraph[N]().add(graph.nodes).link(graph.edges,true)

    /** Graph depth-first visitor interface */
    trait GraphDfsVisitor[@specialized(Int) N] {
        def start(node:N):Unit = {}
        def before(node:N):Unit = {}
	    def edge(edge:(N,N)):Unit = {}
        def after(node:N):Unit = {}
    }

	/** Depth-first search of the whole graph */
	def dfs[@specialized(Int) N](graph:Graph[N], visitor: GraphDfsVisitor[N]):Unit = dfs(graph, visitor, graph.nodes)
	/** Depth-first search of the whole graph in the given node's order*/
	def dfs[@specialized(Int) N](graph:Graph[N], visitor: GraphDfsVisitor[N], nodes:Traversable[N]):Unit = {
		val explored = new mutable.LinkedHashSet[N]()
		for (node <- nodes){
			if (!(explored contains node)){
				visitor start node
				dfsi(graph,node,visitor,explored)
			}
		}
	}
	/** Depth-first search (recursive) of the graph starting at given node */
	def dfs[@specialized(Int) N](graph:Graph[N],node:N, visitor: GraphDfsVisitor[N], explored:mutable.LinkedHashSet[N] = mutable.LinkedHashSet[N]()):Unit = {
		if (!(explored contains node)){
			explored add node
			visitor before node
			for (next <- graph.adjacent(node) if !explored.contains(next)) {
				visitor edge ((node,next))
				dfs(graph,next,visitor,explored)
			}
			visitor after node
		}
	}

    /** Depth-first search (iterative) of the graph starting at given node */
    def dfsi[@specialized(Int) N](graph:Graph[N], source:N, visitor: GraphDfsVisitor[N], explored:mutable.LinkedHashSet[N] = mutable.LinkedHashSet[N]()):Unit = {
        val stack = new mutable.Stack[N]()
	    explored add source
        stack.push(source)
        visitor before source
        while (!stack.isEmpty){
            val node = stack.top
            graph.adjacent(node) find (n => !explored.contains(n)) match {
                case Some(next) => {
	                explored add next
                    stack.push(next)
                    visitor edge ((node,next))
	                visitor before next
                }
                case None => {
                    stack.pop()
                    visitor after node
                }
            }
        }
    }

	/** Breath-first search of the whole graph */
	def bfs[@specialized(Int) N](graph:Graph[N], visitor: N => Unit):Unit = {
		val explored = mutable.LinkedHashSet[N]()
		for (node <- graph.nodes){
			if (!(explored contains node)){
				bfs(graph,node,visitor,explored)
			}
		}
	}

	/** Breath-first search of the graph starting at given node */
	def bfs[@specialized(Int) N](graph:Graph[N], node: N, visitor: N => Unit, explored:mutable.LinkedHashSet[N] = mutable.LinkedHashSet[N]()):Unit = {
		val queue = new mutable.Queue[N]()
		queue.enqueue(node)
		while (!queue.isEmpty){
			val n = queue.dequeue()
			if (!(explored contains n)){
				explored add n
				visitor(n)
				for (next <- graph.adjacent(n)) queue.enqueue(next)
			}
		}
	}

    /** Finds all cycles */
	def findCycles[@specialized(Int) N](graph:Graph[N]): Vector[N] = {
		var cycles: Vector[N] = Vector.empty[N]
		val marks = new mutable.LinkedHashMap[N,Char]().withDefaultValue('0')
		for (node <- graph.nodes if marks(node) == '0') {
			cycles = cycles ++ findCycles(graph,node,marks)
		}
		cycles
	}

    /** Finds cycles starting at the given node */
	def findCycles[@specialized(Int) N](graph:Graph[N], node: N, marks: mutable.Map[N,Char] = new mutable.LinkedHashMap[N,Char]().withDefaultValue('0')): Vector[N] = {
		var cycles: Vector[N] = Vector.empty[N]
		if (marks(node) == 'x') cycles = cycles :+ node
		else if (marks(node) == '0'){
			marks(node) = 'x'
			graph.adjacent(node) foreach {next =>
				cycles = cycles ++ findCycles(graph,next,marks)
			}
			marks(node) = '1'
		}
		cycles
	}

	private object CycleFoundException extends Exception

    /** Checks if graph contains cycles */
	def hasCycles[@specialized(Int) N](graph:Graph[N]): Boolean = {
		val marks = new mutable.LinkedHashMap[N,Char]().withDefaultValue('0')
		def checkCycles(node: N): Unit = {
			if (marks(node) == 'x') throw CycleFoundException
			else if (marks(node) == '0'){
				marks(node) = 'x'
				graph.adjacent(node) foreach checkCycles
				marks(node) = '1'
			}
		}
		try {
			for (node <- graph.nodes if marks(node) == '0') checkCycles(node)
			false
		}
		catch {
			case CycleFoundException => true
		}
	}

    /** Sorts nodes topologically */
	def sortTopologically[@specialized(Int) N](graph: Graph[N]): List[N] = {
		var counter = graph.nodesCount
		var priorities: List[N] = Nil
		val observer = new GraphDfsVisitor[N] {
			override def after(node: N) {
				priorities = node :: priorities
				counter = counter - 1
			}
		}
		dfs(graph,observer)
		priorities
	}

	/** Dijkstra algorithm finds shortest path in directed graph*/ 
	def findShortestPath[@specialized(Int) N, @specialized(Double,Int) V:Numeric](graph: Graph[N] with Weighted[N,V],from: N, to: N): (V,List[(N,N)]) = {
		findShortestPath(graph,from,to,graph.weight)
	}
	
	/** Dijkstra algorithm finds shortest path in directed graph */
	def findShortestPath[@specialized(Int) N, @specialized(Double,Int) V:Numeric](graph: Graph[N],from: N, to: N, weight: (N,N) => V): (V,List[(N,N)]) = {
		val num: Numeric[V] = implicitly[Numeric[V]]
		if(from==to || graph.adjacent(from).isEmpty) return (num.zero,Nil)
		val nodesCount = graph.nodesCount
		val explored = new mutable.LinkedHashSet[N]()
		val distance = new mutable.LinkedHashMap[N,V]()
		val backtrace = new MutableMapGraph[N]()
		implicit val ordering = new Ordering[(N,N,V)] {
			def compare(x: (N, N, V), y: (N, N, V)): Int =  {
				num.toInt(num.minus(num.plus(distance(x._1),x._3),num.plus(distance(y._1),y._3)))
			}
		}
		val outgoingEdges = new MinHeap[(N,N,V)](Math.min(graph.nodesCount,1024))
		var head = from
		explored add from
		distance(from) = num.zero
		var nextEdges = graph.adjacent(from) filterNot explored map (node => (from,node,weight(from,node)))
		outgoingEdges insert nextEdges
		do {
			for ((t,h,w) <- outgoingEdges.extract){
				explored add h
				distance(h) = num.plus(distance(t),w)
				backtrace link (h,t,false)
				outgoingEdges remove (outgoingEdges filter {case (_,node,_) => node == h})
				nextEdges = graph.adjacent(h) filterNot explored map (node => (h,node,weight(h,node)))
				outgoingEdges insert nextEdges
				head = h
			}
		} while (head!=to && !outgoingEdges.isEmpty && explored.size != nodesCount)
		// compute resulting path
		var path: List[(N,N)] = Nil
		if(head==to){
			var next = to
			do {
				val node = next
				next = backtrace.adjacent(node).minBy(n => distance(n))
				val segment = (next,node)
				path = segment :: path
			} while(next!=from)
		}
		(distance(to), path)
	}

	/** Dijkstra algorithm finds all shortest paths starting at given node in directed graph */
	def findShortestPaths[@specialized(Int) N, @specialized(Double,Int) V:Numeric](graph: Graph[N] with Weighted[N,V],from: N): scala.collection.Map[N,V] = {
		findShortestDistances(graph,from,graph.weight)
	}

	/** Dijkstra algorithm finds all shortest paths starting at given node in directed graph */
	def findShortestDistances[@specialized(Int) N, @specialized(Double,Int) V:Numeric](graph: Graph[N],from: N, weight: (N,N) => V): scala.collection.Map[N,V] = {
		val num: Numeric[V] = implicitly[Numeric[V]]
		if(graph.adjacent(from).isEmpty) return Map.empty
		val nodesCount = graph.nodesCount
		val explored = new mutable.LinkedHashSet[N]()
		val distance = new mutable.LinkedHashMap[N,V]()
		implicit val ordering = new Ordering[(N,N,V)] {
			def compare(x: (N, N, V), y: (N, N, V)): Int =  {
				num.toInt(num.minus(num.plus(distance(x._1),x._3),num.plus(distance(y._1),y._3)))
			}
		}
		val outgoingEdges = new MinHeap[(N,N,V)](Math.min(graph.nodesCount,1024))
		var head = from
		explored add from
		distance(from) = num.zero
		var nextEdges = graph.adjacent(from) filterNot explored map (node => (from,node,weight(from,node)))
		outgoingEdges insert nextEdges
		do {
			for ((t,h,w) <- outgoingEdges.extract){
				explored add h
				distance(h) = num.plus(distance(t),w)
				outgoingEdges remove (outgoingEdges.view filter {case (_,node,_) => node == h})
				nextEdges = graph.adjacent(h) filterNot explored map (node => (h,node,weight(h,node)))
				outgoingEdges insert nextEdges
				head = h
			}
		} while (!outgoingEdges.isEmpty && explored.size != nodesCount)
		distance
	}

	/* Kosaraju's 2-dfs pass algorithm finds strongly connected components */
	def findStronglyConnectedComponents[@specialized(Int) N](graph:Graph[N]): Traversable[Traversable[N]] = {
		val reversed: Graph[N] = graph.reverse
		val nodes = mutable.Seq[N]() ++ graph.nodes
		// first dfs pass
		val times = new mutable.LinkedHashMap[N,Int]()
		dfs(reversed, new GraphDfsVisitor[N] {
			var time:Int = 0
			override def after(node:N) {
				time = time + 1
				times(node) = time
			}
		}, nodes)
		// sorting nodes by reversed entry time
		implicit val ordering = new Ordering[N] {
			def compare(x: N, y: N): Int = times(y) - times(x)
		}
		QuickSort.sort(nodes)
		// second dfs pass
		val leaders = new mutable.LinkedHashMap[N,N]()
		dfs(graph, new GraphDfsVisitor[N] {
			var leader: Option[N] = None
			override def start(node:N) {
				leader = Some(node)
			}
			override def before(node:N) {
				leaders(node) = leader.get
			}
		}, nodes)
		// result computing
		val result = (graph.nodes groupBy leaders).toSeq sortBy {case (_,seq) => -seq.size} map {case (_,seq) => seq}
		result
	}

    /** Merge two nodes into one (collapse) */
	def mergeNodes[@specialized(Int) N](g: Graph[N], mergedNode: N, removedNode: N): MutableMapGraph[N] =  {
		val graph:MutableMapGraph[N] = g match {
			case x: MutableMapGraph[N] => x
			case _ => Graph.deepCopy(g)
		}
		//merge two adjacent lists, remove self-loops
		val removedAdjacent = graph.nodeMap(removedNode)
		val mergedAdjacent = graph.nodeMap(mergedNode)
		val newAdjacent = new mutable.ArrayBuffer[N](removedAdjacent.size+mergedAdjacent.size)
		for(node <- mergedAdjacent) {
			if(node != removedNode) newAdjacent += node
		}
		for(node <- removedAdjacent) {
			if(node != mergedNode) newAdjacent += node
		}
		graph.nodeMap -= removedNode //remove node
		graph.nodeMap(mergedNode) = newAdjacent //set new adjacent for mergedNode
		graph.nodeMap transform {
			(_, adjacent) => {
				if (adjacent.contains(removedNode)){
					adjacent map {
						case n if n==removedNode => mergedNode
						case n => n
					}
				} else {
					adjacent
				}
			}
		}
		graph
	}

	def randomize[N](seq:Seq[N]):Seq[N] =  {
		seq
			.map (item => (Math.random(),item))
			.sortBy {case (priority,_) => priority}
			.map {case (_,item) => item}
	}

	def randomCutCount[@specialized(Int) N](g:Graph[N]):Int = {
		val graph:MutableMapGraph[N] = g match {
			case x: MutableMapGraph[N] => x
			case _ => Graph.deepCopy(g)
		}
		val nodesQueue = mutable.Queue[N](randomize(graph.nodes.toSeq):_*)
		while(graph.nodeMap.size>2){
			val node1 = nodesQueue.dequeue()
			val adjacent = graph.nodeMap(node1)
			if(adjacent.size>0){
				val j = (Math.random()*adjacent.size).asInstanceOf[Int]
				val node2 =  adjacent(j)
				mergeNodes(graph,node2,node1)
			}
		}
		val (_, adjacent) = graph.nodeMap.head
		adjacent.size
	}

    /** Set of leaf nodes (without edges beginning at)*/
    def leavesOf[@specialized(Int) N](graph: Graph[N]): Set[N] = {
        (for(node <- graph.nodes if graph.adjacent(node).isEmpty) yield node).toSet
    }

    /** Set of root nodes (without edges leading to) */
    def rootsOf[@specialized(Int) N](graph: Graph[N]): Set[N] = {
        val reversed = graph.reverse
        (for(node <- graph.nodes if reversed.adjacent(node).isEmpty) yield node).toSet
    }

    /** Filters out every one node that does not belong to one of the paths leading to one of the given target nodes **/
    def filterOutPathsNotLeadingTo[@specialized(Int) N](graph: Graph[N], nodes: Traversable[N]): Graph[N] = {
        val newGraph:MutableGraph[N] = Graph()
        val reversed = graph.reverse
        val visitor = new GraphDfsVisitor[N] {
            override def before(node:N):Unit = newGraph.add(node)
            override def edge(edge: (N,N)): Unit = newGraph.link(edge.swap,false)
        }
        for(node <- nodes) {
            Graph.dfsi(reversed,node,visitor)
        }
        newGraph
    }

    /** Peels skin of the graph, returns inner graph and two collections: roots and leaves */
    def peelSkin[@specialized(Int) N](graph: Graph[N]):(Graph[N],Traversable[N],Traversable[N]) = {
       val roots = graph.roots
       val leaves = graph.leaves
       val newGraph = graph.filter(n => !roots.contains(n) && !leaves.contains(n))
       (newGraph,roots,leaves)
    }
}

