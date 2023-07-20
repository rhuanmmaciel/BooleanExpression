package entities.elements;

public class Column extends Element{

    private final String columnName;
    private final String columnSource;

    public Column(String columnName, String columnSource){

        this.columnName = columnName;
        this.columnSource = columnSource;

    }

    public String getColumnName(){
        return columnName;
    }

    public String getColumnSource(){
        return columnSource;
    }

    public String getSourceAndColumn(){
        return columnSource+"."+columnName;
    }

    @Override
    public String toString(){
        return getSourceAndColumn();
    }

}
