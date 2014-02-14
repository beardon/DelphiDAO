{.$DEFINE DeveloperCaution} // Force developer caution compiler directive.
{$DEFINE DevelopmentDB} // Use a development copy of the database.
{.$DEFINE GenerateInterfaces} // Create interfaces and have DAO objects implement them
{.$DEFINE TestDB} // Use a test copy of the database.

{$IFDEF DevelopmentDB}
  {$DEFINE DeveloperCaution}
{$ENDIF}

