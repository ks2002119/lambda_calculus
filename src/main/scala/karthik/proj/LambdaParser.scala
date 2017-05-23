package karthik.proj

/**
  * Created by ksubramanian on 5/15/17.
  */
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

class LambdaParser extends RegexParsers with PackratParsers {
    type Tokens = StdLexical
    val log = Logger("LambdaEvaluator")
    val lexical = new CustomLambdaLexer
    lexical.delimiters ++= Seq("\\", ".", "(", ")")

    lazy val lambdaExpression: PackratParser[LE] = application | otherExpressionTypes
    lazy val otherExpressionTypes                = variable |  parens | lambda
    lazy val lambda: PackratParser[Lambda]     =
        positioned(("\\") ~> variable ~ "." ~ parens ^^ {
                case arg ~ "." ~ body  => {
                    log.debug("Lambda with variable:{} body:{}", arg.toString, body.toString);
                    Lambda(arg, body)
                }
            }
        )
    lazy val application: PackratParser[Apply] =
        positioned(lambdaExpression ~ otherExpressionTypes ^^ {
                case left ~ right => {
                    log.debug("Application with left:{} right:{}", left.toString, right.toString);
                    Apply(left, right)
                }
            }
        )
    lazy val variable: PackratParser[Var] =
        positioned("""[A-Za-z]""".r ^^ ((x: String) => {
            log.debug("Variable:{}", x.toString);
            Var(x.charAt(0).toString)
        }))
    lazy val parens: PackratParser[LE]       = "(" ~> lambdaExpression <~ ")" | lambdaExpression
}

class CustomLambdaLexer extends StdLexical {
//    val reservedTokens = Seq("\\", ".", "(", ")")
    // We only allow alphabets, digits or symbols not allowed.
    override def letter = elem("letter", c => c.isLetter &&
        c != '(' && c != ')' && c != '\\' && c != '.')
}

object SimpleLambdaParser extends LambdaParser {
    def parse(s: CharSequence): LE = {
        parse(new CharSequenceReader(s))
    }

    def parse(input: CharSequenceReader): LE = {
        parsePhrase(input) match {
            case Success(t, _) => t
            case NoSuccess(msg, next) => throw new IllegalArgumentException(
                "Could not parse '" + input + "' near '" + next.pos.longString + ": " + msg)
        }
    }

    def parsePhrase(input: CharSequenceReader): ParseResult[LE] = {
        phrase(lambdaExpression)(input)
    }
}