package examples;

import entities.constructor.CallbackExpressionConstructor;
import entities.constructor.ExpressionConstructor;
import entities.elements.Column;
import entities.elements.Value;
import entities.expressions.AtomicExpression;
import entities.expressions.BooleanExpression;
import entities.expressions.LogicalExpression;
import enums.LogicalOperator;

public class Main {

    public static void main(String[] args) {

        ExpressionConstructor constructor = new ExpressionConstructor();

        AtomicExpression e1 = new AtomicExpression(new Column("nome", "tabela"), new Value("marcos"), "==");
        AtomicExpression e2 = new AtomicExpression(new Column("idade", "tabela"), new Value(5), "!=");
        AtomicExpression e3 = new AtomicExpression(new Column("altura", "tabela"), new Value(178), ">");
        AtomicExpression e4 = new AtomicExpression(new Value("maria"), new Value("marcos"), "==");
        AtomicExpression e5 = new AtomicExpression(new Column("nome", "tabela"), new Column("nome", "tabela2"), "==");
        AtomicExpression e6 = new AtomicExpression(new Column("nome", "tabela"), new Value(null), "is", true);
        AtomicExpression e7 = new AtomicExpression(new Column("nome", "tabela"), new Value("marcos"), "==", true);


        constructor.and(new CallbackExpressionConstructor() {
            @Override
            public void handle(ExpressionConstructor constructor) {
                constructor.and(e4);
                constructor.and(e5);
            }
        });
        constructor.and(new CallbackExpressionConstructor() {
            @Override
            public void handle(ExpressionConstructor constructor) {
                constructor.or(new CallbackExpressionConstructor() {
                    @Override
                    public void handle(ExpressionConstructor constructor) {
                        constructor.or(e1);
                        constructor.or(e2);
                        constructor.or(e3);
                    }
                });
                constructor.or(e6);
            }
        });


        LogicalExpression or1 = new LogicalExpression(LogicalOperator.OR, e1, e2, e3);
        LogicalExpression and1 = new LogicalExpression(LogicalOperator.AND, e4, e5);
        LogicalExpression or2 = new LogicalExpression(LogicalOperator.OR, or1, e6);
        BooleanExpression booleanExpression = new LogicalExpression(LogicalOperator.AND, or2, and1);

        System.out.println(constructor.build());

    }

}