package utils;

import dsl.BooleanExpressionDSLController;
import dsl.BooleanExpressionDSLLexer;
import dsl.BooleanExpressionDSLParser;
import entities.expressions.BooleanExpression;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public class Recognizer {

    public static BooleanExpression recognize(String txt) {
        BooleanExpressionDSLParser parser = new BooleanExpressionDSLParser(
                new CommonTokenStream(new BooleanExpressionDSLLexer(CharStreams.fromString(txt))));

        ParseTreeWalker walker = new ParseTreeWalker();
        BooleanExpressionDSLController listener = new BooleanExpressionDSLController();

        walker.walk(listener, parser.command());

        return null;
    }

}
