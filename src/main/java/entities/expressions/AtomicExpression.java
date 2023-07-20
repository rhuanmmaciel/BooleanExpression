package entities.expressions;

import entities.elements.Column;
import entities.elements.Element;
import enums.RelationalOperator;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class AtomicExpression extends BooleanExpression{

    private final Element firstElement;
    private final Element secondElement;
    private final RelationalOperator relationalOperator;

    public AtomicExpression(Element firstElement, Element secondElement, RelationalOperator relationalOperator){

        this(firstElement, secondElement, relationalOperator, true);

    }

    public AtomicExpression(Element firstElement, Element secondElement, RelationalOperator relationalOperator, boolean booleanValue){

        super(booleanValue);

        this.firstElement = firstElement;
        this.secondElement = secondElement;

        this.relationalOperator = relationalOperator;

    }

    public boolean isFirstElementAColumn(){
        return firstElement instanceof Column;
    }

    public boolean isSecondElementAColumn(){
        return secondElement instanceof Column;
    }

    public boolean hasColumn(){
        return isFirstElementAColumn() || isSecondElementAColumn();
    }

    public List<Column> getMandatoryVariables() {

        if(!hasColumn()) return List.of();

        List<Column> mandatoryVariables = new ArrayList<>();

        if(isFirstElementAColumn())
            mandatoryVariables.add((Column) firstElement);

        if(isSecondElementAColumn())
            mandatoryVariables.add((Column) secondElement);

        return mandatoryVariables;

    }

    public Element getFirstElement() {
        return firstElement;
    }

    public Element getSecondElement() {
        return secondElement;
    }

    public RelationalOperator getRelationalOperator() {
        return relationalOperator;
    }

    @Override
    public String toString(){

        String txt = firstElement + " " + relationalOperator + " " + secondElement;

        if(isFalse()) txt = "!(" + txt + ")";

        return txt;

    }


}
