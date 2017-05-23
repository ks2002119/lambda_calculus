package karthik.proj

import scala.util.parsing.input.Positional

/**
  * Model classes
  */
sealed abstract class LE extends Positional

case class Var(var x: String) extends LE {
    override def toString = x.toString
}

case class Lambda(x:Var, e:LE) extends LE {
    def apply(e: LE) = Apply(this, x)
    override def toString = "\\" + x + "." + e.toString

}

case class Apply(e1: LE, e2: LE) extends LE {
    override def toString() = "(" + e1.toString + " " + e2.toString + ")"
}