package entities.expressions;

import entities.elements.Column;

import java.util.ArrayList;
import java.util.List;

public abstract class BooleanExpression {

    private final boolean booleanValue;

    public BooleanExpression(boolean booleanValue){
        this.booleanValue = booleanValue;
    }

    public boolean hasMandatoryVariables(){

        return (this instanceof AtomicExpression atomic && atomic.hasColumn())
                || (this instanceof LogicalExpression log && log.isAnd() && log.hasAtomicAndColumn());
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

    public boolean isFalse(){
        return !booleanValue;
    }

    public abstract boolean hasColumn();

}
