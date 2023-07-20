package entities.expressions;

import entities.elements.Column;
import enums.LogicalOperator;

import java.util.*;

public class LogicalExpression extends BooleanExpression implements Iterable<BooleanExpression> {

    private final List<BooleanExpression> expressions = new ArrayList<>();
    private final LogicalOperator logicalOperator;

    public LogicalExpression(LogicalOperator logicalOperator, boolean booleanValue, List<BooleanExpression> list) {
        super(booleanValue);
        this.logicalOperator = logicalOperator;

        for (BooleanExpression expression :
                list) {
            if (expression instanceof LogicalExpression logicalExpression) {
                if (logicalExpression.logicalOperator == logicalOperator) {
                    this.expressions.addAll(logicalExpression.expressions);
                    continue;
                }
            }

            this.expressions.add(expression);
        }


    }

    public LogicalExpression(LogicalOperator logicalOperator, List<BooleanExpression> list) {
        this(logicalOperator, true, list);
    }

    public LogicalExpression(LogicalOperator logicalOperator, BooleanExpression... expressions) {
        this(logicalOperator, true, List.of(expressions));
    }

    public LogicalExpression(LogicalOperator logicalOperator, boolean booleanValue, BooleanExpression... expressions) {
        this(logicalOperator, booleanValue, List.of(expressions));
    }

    public LogicalOperator getLogicalOperator() {
        return logicalOperator;
    }

    public boolean isAnd() {
        return LogicalOperator.AND.equals(logicalOperator);
    }

    public boolean isOr() {
        return LogicalOperator.OR.equals(logicalOperator);
    }

    public boolean hasAtomicExp() {
        return expressions.stream().anyMatch(x -> x instanceof AtomicExpression);
    }

    public List<Column> getMandatoryVariables(){

        if(isOr() || !hasAtomicAndColumn()) return List.of();

        List<Column> mandatoryVariables = new ArrayList<>();

        List<BooleanExpression> atomicExpressions = expressions.stream().filter(x -> x instanceof AtomicExpression).toList();

        for(BooleanExpression exp : atomicExpressions)
            if(exp.hasColumn()) mandatoryVariables.addAll(((AtomicExpression)exp).getMandatoryVariables());

        return mandatoryVariables;

    }

    public boolean hasAtomicAndColumn(){

        if(!hasAtomicExp()) return false;

        List<BooleanExpression> atomicExpressions = expressions.stream().filter(x -> x instanceof AtomicExpression).toList();

        for(BooleanExpression exp : atomicExpressions)
            if(exp.hasColumn()) return true;

        return false;

    }

    public boolean hasColumn(){
        for(BooleanExpression exp : expressions){

            if(exp instanceof AtomicExpression atomic && atomic.hasColumn()) return true;

            if(exp instanceof LogicalExpression log && log.hasColumn()) return true;

        }

        return false;
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

        if(isFalse()) txt.append("!");
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
