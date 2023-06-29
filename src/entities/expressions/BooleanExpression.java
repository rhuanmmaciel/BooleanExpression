package entities.expressions;

public abstract class BooleanExpression {

    private final boolean not;

    public BooleanExpression(boolean not){
        this.not = not;
    }

    public boolean isNot(){
        return not;
    }

}
