{ $Id$ }
unit DAOFactory;

interface

uses
  Connection,
${uses_list}

type
  {**
   * DAOFactory
   * @author: Aaron Bean
   * @date: ${date}
   *}
  TDAOFactory = class
  private
    class var FConnection: TConnection;
  public
    class constructor Create;
    class destructor Destroy;
${function_declarations}
  end;
  
implementation

class constructor TDAOFactory.Create;
begin
  FConnection := TConnection.Create;
end;

class destructor TDAOFactory.Destroy;
begin
  FConnection.Close;
  FConnection.Free;
end;

${implementation_code}

end.