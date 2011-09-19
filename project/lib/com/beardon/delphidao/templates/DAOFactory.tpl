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
    class var fConnection: TConnection;
  public
    class constructor Create;
    class destructor Destroy;
${function_declarations}
  end;
  
implementation

class constructor TDAOFactory.Create;
begin
  fConnection := TConnection.Create;
end;

class destructor TDAOFactory.Destroy;
begin
  fConnection.Close;
  fConnection.Free;
end;

${implementation_code}

end.