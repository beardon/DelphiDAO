{ $Id$ }
unit DAOFactory;

interface

uses
  ConnectionExt,
${uses_list}

type
  {**
   * DAOFactory
   * @author: Aaron Bean
   *}
  TDAOFactory = class
  private
    class var FConnection: TConnectionExt;
  public
    class constructor Create;
    class destructor Destroy;
${function_declarations}
  end;
  
implementation

class constructor TDAOFactory.Create;
begin
  FConnection := TConnectionExt.Create;
end;

class destructor TDAOFactory.Destroy;
begin
  FConnection.Close;
  FConnection.Free;
end;

${implementation_code}

end.