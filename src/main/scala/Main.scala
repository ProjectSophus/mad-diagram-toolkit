package io.github.projectsophus.diagramtoolkit

import Expression._

object Main extends App {
    
    val exp : Output = (In(0, "7").fact * In(1, "6") * In(2, "3")) --> "16"
    
    println(exp.toTex)
    
}
