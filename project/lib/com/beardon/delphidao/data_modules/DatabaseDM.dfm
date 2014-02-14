object DatabaseDataModule: TDatabaseDataModule
  OldCreateOrder = False
  Height = 150
  Width = 215
  object DBConnection: TMyConnection
    Database = 'schema'
    ConnectionTimeout = 60
    Username = 'developer'
    Server = 'localhost'
    LoginPrompt = False
    Left = 32
    Top = 8
  end
end
