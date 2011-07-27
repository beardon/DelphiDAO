object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'DelphiDAO - Beardon Services, Inc.'
  ClientHeight = 55
  ClientWidth = 438
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  DesignSize = (
    438
    55)
  PixelsPerInch = 96
  TextHeight = 16
  object btnGenerate: TButton
    Left = 350
    Top = 16
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Generate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btnGenerateClick
  end
  object edtPath: TEdit
    Left = 16
    Top = 16
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    TextHint = 'Path to generate DAO objects'
  end
  object btnDir: TButton
    Left = 316
    Top = 16
    Width = 21
    Height = 24
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 2
    OnClick = btnDirClick
  end
end
