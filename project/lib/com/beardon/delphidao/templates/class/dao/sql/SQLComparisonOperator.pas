unit SQLComparisonOperator;

interface

type
  TSQLComparisonOperator = class
  public
    const INDEX_OPERATOR_MAP: array[0..4] of string = ('=', '!=', 'LIKE', '<', '>');
    const EQUAL = 0;
    const NOT_EQUAL = 1;
    const LIKE = 2;
    const LESS_THAN = 3;
    const GREATER_THAN = 4;
  end;

implementation

end.
