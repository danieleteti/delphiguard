object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DGuard :: Sample'
  ClientHeight = 291
  ClientWidth = 740
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object btnGuard: TButton
    Left = 8
    Top = 8
    Width = 193
    Height = 65
    Caption = 'Indipendent Objects'
    TabOrder = 0
    OnClick = btnGuardClick
  end
  object btnMultiple: TButton
    Left = 8
    Top = 79
    Width = 193
    Height = 65
    Caption = 'Group Lifetime Management'
    TabOrder = 1
    OnClick = btnMultipleClick
  end
end
