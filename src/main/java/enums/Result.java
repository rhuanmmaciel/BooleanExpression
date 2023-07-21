package enums;

public enum Result {
    TRUE,
    FALSE,
    NOT_READY;

    public static Result evaluate(Boolean condition){

        if(condition) return TRUE;
        return FALSE;

    }

}
