object FormDemo: TFormDemo
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'FormDemo'
  ClientHeight = 664
  ClientWidth = 938
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 25
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 938
    Height = 62
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    TabOrder = 0
    object btnSave: TButton
      Left = 792
      Top = 14
      Width = 113
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Save'
      TabOrder = 0
      OnClick = btnSaveClick
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 62
    Width = 938
    Height = 602
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alClient
    TabOrder = 1
    object pnlDatabase1: TPanel
      Left = 0
      Top = 22
      Width = 445
      Height = 394
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
      object lblPassword1: TLabel
        Left = 35
        Top = 99
        Width = 75
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Password'
      end
      object lblPort1: TLabel
        Left = 60
        Top = 219
        Width = 32
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Port'
      end
      object lblServer1: TLabel
        Left = 46
        Top = 163
        Width = 49
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Server'
      end
      object lblUser1: TLabel
        Left = 60
        Top = 51
        Width = 35
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'User'
      end
      object sePort1: TSpinEdit
        Left = 120
        Top = 216
        Width = 182
        Height = 36
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object edtPassword1: TEdit
        Left = 120
        Top = 91
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 1
        Text = 'edtPassword1'
      end
      object edtServer1: TEdit
        Left = 120
        Top = 160
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 2
        Text = 'edtServer1'
      end
      object edtUser1: TEdit
        Left = 120
        Top = 48
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 3
        Text = 'edtUser1'
      end
    end
    object pnlDatabase2: TPanel
      Left = 480
      Top = 22
      Width = 445
      Height = 394
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 1
      object lblPassword2: TLabel
        Left = 35
        Top = 99
        Width = 75
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Password'
      end
      object lblPort2: TLabel
        Left = 60
        Top = 219
        Width = 32
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Port'
      end
      object lblServer2: TLabel
        Left = 46
        Top = 163
        Width = 49
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Server'
      end
      object lblUserName: TLabel
        Left = 60
        Top = 51
        Width = 35
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'User'
      end
      object sePort2: TSpinEdit
        Left = 120
        Top = 216
        Width = 182
        Height = 36
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object edtPassword2: TEdit
        Left = 120
        Top = 91
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 1
        Text = 'Pass 2'
      end
      object edtServer2: TEdit
        Left = 120
        Top = 160
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 2
        Text = 'srv 2'
      end
      object edtUser2: TEdit
        Left = 120
        Top = 48
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 3
        Text = 'User 2'
      end
    end
  end
end
