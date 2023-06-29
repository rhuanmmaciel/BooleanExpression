package entities.expressions;

import entities.elements.Element;

import java.util.List;

public class AtomicExpression extends BooleanExpression{

    public final static List<String> RELATIONAL_OPERATORS = List.of("<", ">", ">=", "<=", "==", "!=", "is", "is not");

    private final Element firstElement;
    private final Element secondElement;
    private final String relationalOperator;

    public AtomicExpression(Element firstElement, Element secondElement, String relationalOperator){

        super(false);

        this.firstElement = firstElement;
        this.secondElement = secondElement;

        isValidRelationOperator(relationalOperator);

        this.relationalOperator = relationalOperator;


    }

    public AtomicExpression(Element firstElement, Element secondElement, String relationalOperator, boolean not){

        super(not);

        this.firstElement = firstElement;
        this.secondElement = secondElement;

        isValidRelationOperator(relationalOperator);

        this.relationalOperator = relationalOperator;

    }

    public static void isValidRelationOperator(String relationalOperator){

        RELATIONAL_OPERATORS.stream().filter(relationalOperator::equals).findFirst().orElseThrow(IllegalArgumentException::new);

    }

    public Element getFirstElement() {
        return firstElement;
    }

    public Element getSecondElement() {
        return secondElement;
    }

    public String getRelationalOperator() {
        return relationalOperator;
    }

    @Override
    public String toString(){
        return firstElement + " " + relationalOperator + " " + secondElement;
    }

}
