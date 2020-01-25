package io.github.projectsophus.diagramtoolkit

object NodeAndPath {
    class Node (val text : String, val box : Boolean) {
        
        def toTex : String = raw"\node[${if(box) "rectangle,draw" else ""}]($name) at ($x, $y) {$text};"
        
        var x : Double = 0
        var y : Double = 0
        var name : String = null
        
        var order : Int = 0
        var column : Int = 0
        
    }
    
    class Path (val from : Node, val to : Node) {
        
        def toTex : String = raw"\draw[->] ($fromName) -- ($toName);"
        
        var fromName : String = null
        var toName : String = null
        
    }
    
    case class Structure (nodes : Seq[Node], paths : Seq[Path]) {
        
        def toTex() : String = {
            populate()
            val nodesText = nodes.map(_.toTex).mkString("\n    ")
            val pathsText = paths.map(_.toTex).mkString("\n    ")
            
        raw"""
        |\begin{figure}
        |  \centering
        |  \begin{tikzpicture}
        |    $nodesText
        |
        |    $pathsText
        |  \end{tikzpicture}
        |\end{figure}""".stripMargin
        }
        
        def populate() = {
            populateColumns()
            populateXPosition()
            populateOrder()
            populateYPosition()
            populateName()
        }
        
        def compInitialNodes = nodes.filter(node => !paths.exists(_.to == node))
        def compFinalNodes = nodes.filter(node => !paths.exists(_.from == node))
        def compChildren(parent : Node) = nodes.filter(node => paths.exists(p => p.from == parent && p.to == node))
        def compParents(child : Node) = nodes.filter(node => paths.exists(p => p.from == node && p.to == child))
        
        def compDepth = nodes.map(_.column).max
        
        def populateColumns() = {
            
            val finalNodes = compFinalNodes
            
            finalNodes.foreach(process(0))
            
            def process (column : Int)(node : Node) : Unit = {
                node.column = Math.min(column, node.column)
                
                val nextNodes = compParents(node)
                
                nextNodes.foreach(process(column - 1))
                
            }
            
            val depth = nodes.map(_.column).min
            
            for {node <- nodes} {
                node.column -= depth
            }
            
            /*
            val initialNodes = compInitialNodes
            
            initialNodes.foreach(process(0))
            
            def process (column : Int)(node : Node) : Unit = {
                node.column = Math.max(column, node.column)
                
                val nextNodes = compChildren(node)
                
                nextNodes.foreach(process(column + 1))
                
            }
            */
        }
        
        def populateXPosition() = { // TODO: Improve, use width
            
            for (node <- nodes) {
                node.x = node.column * 3
            }
            
        }
        
        class Counter (amtCounters : Int) {
            val counters = collection.mutable.Seq.fill(amtCounters)(0)
            def next(i : Int) = {counters(i) += 1; counters(i) - 1}
        }
        
        def populateOrder() = {
            
            val counter = new Counter(compDepth + 1)
            
            val finalNodes = compFinalNodes
            
            process(finalNodes)
            
            def process(seq : Seq[Node]) : Unit = {
                for (node <- seq) {
                    node.order = counter.next(node.column)
                    
                    process(compParents(node))
                }
            }
            
        }
        
        def populateYPosition() = { // TODO: Improve, use height
            
            for (column <- 0 to compDepth) {
                val cnodes = nodes.filter(_.column == column)
                val amt = cnodes.length
                
                // (0, 1) -> 0
                // (0, 2) -> -2
                // (1, 2) -> 2
                // (0, 3) -> -4
                // (1, 3) -> 0
                // (2, 3) -> 4
                
                val dist = 2
                
                for (node <- cnodes) {
                    node.y = dist * ( -(amt - 1) / 2.0 + node.order)
                }
                
            }
            
        }
        
        def populateName() = {
            def nameOf(node : Node) = s"col${node.column}row${node.order}"
            
            for (node <- nodes) {
                node.name = nameOf(node)
            }
            
            for (path <- paths) {
                path.fromName = nameOf(path.from)
                path.toName = nameOf(path.to)
            }
            
        }
        
    }
    
}
