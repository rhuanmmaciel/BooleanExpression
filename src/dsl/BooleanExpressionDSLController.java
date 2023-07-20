package dsl;

public class BooleanExpressionDSLController extends BooleanExpressionDSLBaseListener {

    @Override
    public void exitCommand(BooleanExpressionDSLParser.CommandContext ctx) {
        System.out.println(ctx.getText());
    }
}

