{ $Id$ }
unit stored_routines;

interface

uses
  DBClient,
  query,
  query_executor;

type
  {**
   * Stored Routines
   * @author: Aaron Bean
   * @date: ${date}
   *}
  TStoredRoutines = class
  public
${function_declarations}
  end;
  
implementation

${implementation_code}

end.