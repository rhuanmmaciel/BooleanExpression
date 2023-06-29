package entities.expressions;

import enums.LogicalOperator;

import java.util.*;

public class LogicalExpression extends BooleanExpression implements Iterable<BooleanExpression>{

    private final List<BooleanExpression> expressions = new ArrayList<>();
    private final LogicalOperator logicalOperator;

    public LogicalExpression(LogicalOperator logicalOperator,boolean not, List<BooleanExpression> list){
        super(not);
        this.logicalOperator = logicalOperator;

        for (BooleanExpression expression:
             list) {
            if(expression instanceof LogicalExpression){
                LogicalExpression logicalExpression = (LogicalExpression) expression;
                if(logicalExpression.logicalOperator==logicalOperator){
                    this.expressions.addAll(logicalExpression.expressions);
                    continue;
                }
            }

            this.expressions.add(expression);
        }



    }
    public LogicalExpression(LogicalOperator logicalOperator, List<BooleanExpression> list){
        this(logicalOperator,false,list);
    }
    public LogicalExpression(LogicalOperator logicalOperator, BooleanExpression... expressions){
        this(logicalOperator,false,List.of(expressions));
    }

    public LogicalExpression(LogicalOperator logicalOperator, boolean not, BooleanExpression... expressions){
        this(logicalOperator,not,List.of(expressions));
    }

    public LogicalOperator getLogicalOperator(){
        return logicalOperator;
    }

    public boolean isAnd(){
        return LogicalOperator.AND.equals(logicalOperator);
    }

    public boolean isOr(){
        return LogicalOperator.OR.equals(logicalOperator);
    }

    public List<BooleanExpression> getExpressions(){
        return Collections.unmodifiableList(expressions);
    }

    @Override
    public Iterator<BooleanExpression> iterator() {
        return new ExpressionIterator();
    }

    private class ExpressionIterator implements Iterator<BooleanExpression> {
        private int currentIndex;

        public ExpressionIterator() {
            this.currentIndex = 0;
        }

        @Override
        public boolean hasNext() {
            return currentIndex < expressions.size();
        }

        @Override
        public BooleanExpression next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            BooleanExpression element = expressions.get(currentIndex);
            currentIndex++;
            return element;
        }
    }

    @Override
    public String toString() {

        StringBuilder txt = new StringBuilder();

        if(isNot()) txt.append("!");
        txt.append("(");

        Iterator<BooleanExpression> iterator = expressions.iterator();
        while(iterator.hasNext()){

            txt.append(iterator.next());

            if(iterator.hasNext()) txt.append(" ").append(logicalOperator).append(" ");

        }

        txt.append(")");

        return txt.toString();

    }

}
