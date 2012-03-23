{ $Id$ }
unit ${unit_name};

interface

uses
  Classes;

type
  ${pointer_type_name} = ^${type_name};
  {**
   * Class that represents table '${table_name}'.
   *
   * @author: Aaron Bean
   * @date: ${date}
   *}
  ${type_name} = class(TPersistent)
  private
${private_vars}
  public
${public_constants}
    procedure Assign(${type_param_name}: TPersistent); override;
${public_properties}
  end;

implementation

procedure ${type_name}.Assign(${type_param_name}: TPersistent);
begin
  if (${type_param_name} is ${type_name}) then
  begin
${assign_assignments}
  end
  else
    inherited Assign(${type_param_name});
end;

end.