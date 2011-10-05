unit SQLOrderDirection;

interface

type
  TSQLOrderDirection = class
  public
    const INDEX_DIRECTION_MAP: array[0..1] of string = ('ASC', 'DESC');
    const ASCENDING = 0;
    const DESCENDING = 1;
  end;

implementation

end.
