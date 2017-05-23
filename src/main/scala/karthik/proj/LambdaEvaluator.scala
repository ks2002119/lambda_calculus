package karthik.proj

import com.typesafe.scalalogging.Logger
import karthik.proj.SimpleLambdaParser._

import scala.collection.mutable.{HashMap, Map}

/**
  * Created by ksubramanian on 5/16/17.
  */
object LambdaEvaluator {
    val log = Logger("LambdaEvaluator")
    val RECURSION : String = "__recursion__"
    val MAX_LEVEL = 30
    type StringMap = Map[String, String]
    var idGenerator = 0;
    def getNextId = {
        idGenerator += 1;
        idGenerator;
    }

    def evaluate(expression: String) : LE = {
        evaluate(parse(expression))
    }

    def evaluate(ast: LE) : LE = {
        var input = ast;
        val gamma = alphaConversion(input, new Gamma())
        log.debug("After alpha conversion:{}", input.toString)
        var result = betaReduce(input, gamma)
        var count = 1;
        for (count <- 1 to MAX_LEVEL if result._1 != input && !result._2.isExceeded) {
            input = result._1;
            result = betaReduce(input, result._2)
        }
        if (result._2.isExceeded) {
            throw new EvaluatorException("Max recursion depth exceeded. Possibly diverging " +
                "expression")
        }
        rename(result._1, gamma.reverse())
    }

    private def alphaConversion(ast: LE, gamma: Gamma) : Gamma = {
        ast match {
            case v @ Var(x)  => {
                log.debug("Handling variable")
                if (gamma.contains(x) && Set(x) != gamma(x)) {
                    val alphaRenaming = gamma(x).head
                    log.debug("Replacing bound variable:{} with {}", x, alphaRenaming)
                    v.x = alphaRenaming
                }
                gamma
            }
            case Apply(x: LE, y:LE) => {
                log.debug("Handling application")
                val leftGamma = alphaConversion(x, gamma)
                val rightGamma = alphaConversion(y, gamma)
                new Gamma((leftGamma ++ rightGamma))
            }
            case l @ Lambda(Var(x), body:LE) => {
                log.debug("Handling lambda")
                if (!gamma.contains(x)) {
                    log.debug("Adding bound variable {} to gamma", x)
                    gamma += (x -> x)
                } else {
                    val renamedVar = "var" + getNextId
                    log.debug("Renaming bound variable {} to {} in gamma", x, renamedVar)
                    gamma += (x -> renamedVar)
                    l.x.x = renamedVar
                }
                alphaConversion(body, gamma)
            }
        }
    }

    def substitute(body: LE, substitution : (LE, LE)) : LE = {
        body match {
            case v @ Var(_) => {
                if (v == substitution._1) {
                    substitution._2
                } else {
                    v
                }
            }
            case Lambda(x, body) => {
                Lambda(substitute(x, substitution).asInstanceOf[Var],
                    substitute(body, substitution))
            }
            case Apply(x, y) => {
                Apply(substitute(x, substitution), substitute(y, substitution))
            }
        }
    }

    def betaReduce(ast: LE, gamma: Gamma): (LE, Gamma)= {
        if (gamma.isExceeded) {
            (ast, gamma)
        } else {
            gamma.inc()
            val result : (LE, Gamma) = ast match {
                case v@Var(_) => {
                    log.debug("Nothing further to reduce in {}", v.toString)
                    (v, gamma)
                }
                case l@Lambda(Var(_), body: LE) => {
                    val reducedBody = betaReduce(body, gamma)
                    log.debug("Reduced body of lambda:{} to {}", l.toString, reducedBody._1.toString)
                    (Lambda(l.x, reducedBody._1), reducedBody._2)
                }
                case a @ Apply(x, y) => {
                    val reduceY = betaReduce(y, gamma)
                    x match {
                        case Var(_) => (Apply(x, reduceY._1), reduceY._2)
                        case l @ Lambda(arg, body) => (substitute(body, (arg, reduceY._1)), reduceY._2);
                        case Apply(a, b) => {
                            val reduceX = betaReduce(x, gamma)
                            (Apply(reduceX._1, reduceY._1), new Gamma((reduceX._2 ++ reduceY._2)))
                        }
                    }
                }
            }
            gamma.dec()
            result
        }
    }

    private def recurseIfProceeding(l: LE, gamma: Gamma, substituted: LE) = {
        if (substituted == l) {
            (substituted, gamma)
        } else {
            betaReduce(substituted, gamma)
        }
    }

    def rename(le: LE, gamma: StringMap): LE = {
        le match {
            case v @ Var(_) => {
                if (gamma.contains(v.x)) {
                    v.x = gamma(v.x)
                }
                v
            }
            case Lambda(x, e) => {
                Lambda(rename(x, gamma).asInstanceOf[Var], rename(e, gamma))
            }
            case Apply(x, y) => {
                Apply(rename(x, gamma), rename(y, gamma))
            }
        }
    }

    def depth(le: LE) : Int = {
        le match {
            case Var(_) => 1
            case Lambda(Var(_), body: LE) => 1 + depth(body)
            case Apply(x, y) => 1 + Math.max(depth(x), depth(y))
        }
    }

    class Gamma(data : Map[String, Set[String]]) extends HashMap[String, Set[String]] {
        private var recursionCount = 0;
        private var exceeded = false;
        if (data != null) {
            for ((k, v) <- data) {
                put(k, v)
            }
        }
        def this() {
            this(null)
        }
        def inc() = {
            recursionCount = recursionCount + 1;
            if (recursionCount >= MAX_LEVEL) {
                exceeded = true;
            }
        }
        def dec() = {recursionCount = recursionCount - 1;}
        def isExceeded = exceeded
        def +=(kv: (String, String)): Gamma.this.type = super.+=((kv._1, Set(kv._2)))
        def reverse() : Map[String, String] = for ((k,v) <- this; setVal <- v) yield (setVal, k)
    }
}
