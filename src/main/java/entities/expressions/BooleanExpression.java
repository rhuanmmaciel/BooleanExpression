package entities.expressions;

import entities.elements.Column;
import entities.elements.Value;
import enums.LogicalOperator;
import enums.RelationalOperator;
import enums.Result;

import java.beans.Expression;
import java.util.*;

public abstract class BooleanExpression {

    private final boolean booleanValue;

    private final Map<Column, Object> columns = new HashMap<>();

    public BooleanExpression(boolean booleanValue){
        this.booleanValue = booleanValue;
    }

    public boolean hasMandatoryVariables(){

        return (this instanceof AtomicExpression atomic && atomic.hasColumn())
                || (this instanceof LogicalExpression log && log.isAnd() && log.hasAtomicAndColumn());
    }

    public List<Column> getAllColumns(){

        List<Column> allColumns = new ArrayList<>();

        if(this instanceof AtomicExpression atomic && atomic.hasColumn())
            allColumns.addAll(atomic.getAllColumns());

        if(this instanceof LogicalExpression logical){

            List<BooleanExpression> levelExpression = new ArrayList<>(List.of(logical));

            while (!levelExpression.isEmpty()){

                List<BooleanExpression> newLevelExpression = new ArrayList<>();
                for(BooleanExpression exp : levelExpression){

                    if(exp instanceof AtomicExpression atomic && atomic.hasColumn())
                        allColumns.addAll(atomic.getAllColumns());

                    if(exp instanceof LogicalExpression subLogical)
                        newLevelExpression.addAll(subLogical.getExpressions());

                }

                levelExpression = newLevelExpression;

            }

        }

        return allColumns;

    }

    public List<Column> getMandatoryVariables(){

        if(!hasMandatoryVariables()) return List.of();

        List<Column> mandatoryVariables = new ArrayList<>();

        if(this instanceof AtomicExpression atomic)
            mandatoryVariables.addAll(atomic.getMandatoryVariables());
        else
            mandatoryVariables.addAll(((LogicalExpression)this).getMandatoryVariables());

        return mandatoryVariables;

    }

    public Result solve(){

        if(this instanceof AtomicExpression atomic)
            return solveAtomic(atomic);

        LogicalExpression logical = (LogicalExpression) this;

        if(logical.isOr()) return solveLogical(logical, LogicalOperator.OR);

        return solveLogical(logical, LogicalOperator.AND);

    }

    public Result solveLogical(LogicalExpression expression, LogicalOperator operator){

        Result anticipatedResult = Objects.equals(operator, LogicalOperator.OR) ? Result.TRUE : Result.FALSE;

        List<AtomicExpression> atomics = expression.getExpressions().stream().filter(x -> x instanceof AtomicExpression)
                .map(x -> (AtomicExpression)x).toList();

        boolean hasNotReadyVariables = false;

        for(AtomicExpression atomic : atomics) {

            Result atomicResult = solveAtomic(atomic);

            if(Objects.equals(atomicResult, anticipatedResult)) return anticipatedResult;
            if(Objects.equals(atomicResult, Result.NOT_READY)) hasNotReadyVariables = true;

        }

        List<LogicalExpression> logicals = expression.getExpressions().stream().filter(x -> x instanceof LogicalExpression)
                .map(x -> (LogicalExpression)x).toList();

        for(LogicalExpression logical : logicals){

            Result logicalResult = logical.isOr() ? solveLogical(logical, LogicalOperator.OR) : solveLogical(logical, LogicalOperator.AND);;

            if(Objects.equals(logicalResult, anticipatedResult)) return anticipatedResult;
            if(Objects.equals(logicalResult, Result.NOT_READY)) hasNotReadyVariables = true;

        }

        return hasNotReadyVariables ? Result.NOT_READY : (Objects.equals(anticipatedResult, Result.TRUE) ? Result.FALSE : Result.TRUE);

    }

    public Result solveAtomic(AtomicExpression expression){

        if(expression.hasColumn()){
            if(expression.isFirstElementAColumn() && !columnHasValue((Column) expression.getFirstElement()))
                return Result.NOT_READY;
            if(expression.isSecondElementAColumn() && !columnHasValue((Column) expression.getSecondElement()))
                return Result.NOT_READY;
        }

        Object obj1 = expression.isFirstElementAColumn() ? columns.get(expression.getFirstElement()) :
                ((Value)expression.getFirstElement()).getValue();

        Object obj2 = expression.isSecondElementAColumn() ? columns.get(expression.getSecondElement()) :
                ((Value)expression.getSecondElement()).getValue();

        if(!Objects.equals(obj1.getClass(), obj2.getClass())) return Result.FALSE;


        if (obj1 instanceof Comparable) {
            int compareResult = ((Comparable) obj1).compareTo(obj2);

            return switch (expression.getRelationalOperator()) {
                case LESS_THAN -> Result.evaluate(compareResult < 0);
                case GREATER_THAN -> Result.evaluate(compareResult > 0);
                case GREATER_THAN_OR_EQUAL -> Result.evaluate(compareResult >= 0);
                case LESS_THAN_OR_EQUAL -> Result.evaluate(compareResult <= 0);
                case EQUAL -> Result.evaluate(compareResult == 0);
                case NOT_EQUAL -> Result.evaluate(compareResult != 0);
                case IS -> Result.evaluate(true);
                case IS_NOT -> Result.evaluate(false);
            };
        } else {
            throw new UnsupportedOperationException("Objects are not comparable");
        }

    }

    public void insertColumnValue(Column column, Object value){

        getAllColumns().stream().filter(x -> x.equals(column)).findAny().orElseThrow(NoSuchElementException::new);
        columns.put(column, value);

    }

    public boolean columnHasValue(Column column){

        return columns.get(column) != null;

    }

    public boolean isFalse(){
        return !booleanValue;
    }

    public abstract boolean hasColumn();

}
