package karthik.proj

/**
  * Created by ksubramanian on 5/4/17.
  */
import SimpleLambdaParser._
import org.scalatest._

class LanguageSpec extends FlatSpec with Matchers {
    "Lambda Parser" should "parse abstraction lambda expression" in {
        val expression = "\\s.a\n"
        val lambdaExpression = parse(expression);
        assert(lambdaExpression == Lambda(Var("s"), Var("a")))
    }

    it should "parse application lambda expression" in {
        val expression = "(s a)\n"
        val lambdaExpression = parse(expression);
        assert(lambdaExpression == Apply(Var("s"), Var("a")))
    }

    it should "parse complex expression including abstraction and application" in {
        val expression = "\\x.(xx)a\n"
        val lambdaExpression = parse(expression)
        assert(lambdaExpression == Apply(Lambda(Var("x"),Apply(Var("x"),Var("x"))),Var("a")))
    }
}
