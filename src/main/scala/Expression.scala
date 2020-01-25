package io.github.projectsophus.diagramtoolkit

import Expression._

import scala.language.dynamics

abstract sealed class Expression extends Dynamic {
    def f(func : String) = UnaryOp(this, func)
    def applyDynamic(op : String)(other : Expression) = BinaryOp(this, op, other)
    def selectDynamic(op : String) = UnaryOp(this, op)
    
    def +(other : Expression) = BinaryOp(this, "+", other)
    def -(other : Expression) = BinaryOp(this, "-", other)
    def *(other : Expression) = BinaryOp(this, "$\\times$", other)
    
    def -->(out : String) = Output(this, out)
}

object Expression {
    
    case class In(id : Int, value : String) extends Expression
    case class UnaryOp(in : Expression, func : String) extends Expression
    case class BinaryOp(in1 : Expression, op : String, in2 : Expression) extends Expression
    
    case class Output(exp : Expression, out : String) {
        
        def toTex() : String = toNodeAndPath.toTex()
        
        import NodeAndPath._
        
        def toNodeAndPath : Structure = {
            
            val nodes = collection.mutable.Buffer[Node]()
            val paths = collection.mutable.Buffer[Path]()
            
            val inputNodes = collection.mutable.HashMap[Int, Node]()
            
            def process (exp : Expression) : Node = exp match {
                case In(id, value) => {
                    if (inputNodes.contains(id)) {
                        return inputNodes.get(id).get
                    }
                    
                    val newNode = new Node(value, false)
                    inputNodes.addOne(id, newNode)
                    nodes += newNode
                    newNode
                }
                
                case UnaryOp(in, func) => {
                    val newNode = new Node(func, true)
                    nodes += newNode
                    val inNode = process(in)
                    paths += new Path(inNode, newNode)
                    newNode
                }
                
                case BinaryOp(in1, op, in2) => {
                    val newNode = new Node(op, true)
                    nodes += newNode
                    val in1Node = process(in1)
                    val in2Node = process(in2)
                    paths += new Path(in1Node, newNode)
                    paths += new Path(in2Node, newNode)
                    newNode
                }
            }
            
            val expNode = process(exp)
            val outNode = new Node(out, false)
            nodes += outNode
            paths += new Path(expNode, outNode)
            
            Structure(nodes.toSeq, paths.toSeq)
            
        }
        
    }
    
}
