package karthik.proj

/**
  * Created by ksubramanian on 5/4/17.
  */
import karthik.proj.LambdaEvaluator._
import org.scalatest._

class EvaluatorSpec extends FlatSpec with Matchers {

    "Lambda evaluator" should "evaluate abstraction lambda expression" in {
        testExpression("\\s.a\n", Lambda(Var("s"), Var("a")));
    }

    it should "parse application lambda expression" in {
        testExpression("(s a)\n", Apply(Var("s"), Var("a")));
    }

    it should "parse complex expression including abstraction and application" in {
        testExpression("\\x.(xx)a\n", Apply(Var("a"), Var("a")));
    }

    it should "parse expression (λx.(λx.x)) y to λx.x" in {
        testExpression("(\\x.(\\x.x))y\n", Lambda(Var("x"), Var("x")));
    }

    it should "parse divergent combinator (λx.xx)(λx.xx)" in {
        testExpression("(\\x.xx)(\\x.xx)\n", Apply(Lambda(Var("x"), Apply(Var("x"), Var("x"))),
            Lambda(Var("x"), Apply(Var("x"), Var("x")))))
    }

    it should "parse ((λx.(x y))(λz.z)) to y" in {
        testExpression("((\\x.(x y))(\\z.z))", Var("y"));
    }

    it should "parse ((λx.((λy.(x y))x))(λz.w)) to w" in {
        testExpression("((\\x.((\\y.(x y))x))(\\z.w))", Var("w"));
    }

    it should "parse ((((λf.(λg.(λx.((fx)(g x)))))(λm.(λn.(n m))))(λn.z))p) to (z p)" in {
        testExpression("((((\\f.(\\g.(\\x.((fx)(g x)))))(\\m.(\\n.(n m))))(\\n.z))p)",
            Apply(Var("z"), Var("p")));
    }

    it should "parse ((λf.((λg.((f f)g))(λh.(k h))))(λx.(λy.y))) to (λh.(kh))" in {
        testExpression("((\\f.((\\g.((f f)g))(\\h.(k h))))(\\x.(\\y.y)))",
            Lambda(Var("h"), Apply(Var("k"), Var("h"))));
    }

    def testExpression(expression: String, expected: LE) = {
        val result = evaluate(expression);
        assert(result == expected);
    }
}
