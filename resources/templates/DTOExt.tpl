{ $Id$ }
unit ${unit_name};

interface

uses
${uses_list}

type
  ${pointer_type_name} = ^${type_name};
  {**
   * Class that represents database table '${table_name}'.
   * This class will not be overwritten.
   *
   * @author: Aaron Bean
   *}
  ${type_name} = class(${ancestor_type_name})
  protected
  public
  end;

implementation

end.
