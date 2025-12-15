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
    object btnLoad: TButton
      Left = 644
      Top = 14
      Width = 113
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Load'
      TabOrder = 1
      OnClick = btnLoadClick
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
      Top = 10
      Width = 445
      Height = 394
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
      object lblPassword1: TLabel
        Left = 35
        Top = 178
        Width = 75
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Password'
      end
      object lblPort1: TLabel
        Left = 63
        Top = 271
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
        Top = 224
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
        Top = 135
        Width = 35
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'User'
      end
      object lblDatabase1: TLabel
        Left = 35
        Top = 24
        Width = 89
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Database 1'
      end
      object lblDbType1: TLabel
        Left = 73
        Top = 89
        Width = 37
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Type'
      end
      object sePort1: TSpinEdit
        Left = 120
        Top = 268
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
        Top = 175
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 1
        Text = 'pass1'
      end
      object edtServer1: TEdit
        Left = 120
        Top = 221
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
        Top = 132
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 3
        Text = 'User 1'
      end
      object cmbDbType1: TComboBox
        Left = 120
        Top = 89
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = 'MySQL'
        Items.Strings = (
          'MySQL'
          'SQL Server'
          'PostgreSQL'
          'Firebird')
      end
      object chkActive1: TCheckBox
        Left = 120
        Top = 53
        Width = 146
        Height = 26
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Active'
        TabOrder = 5
      end
    end
    object pnlDatabase2: TPanel
      Left = 455
      Top = 10
      Width = 445
      Height = 394
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 1
      object lblPassword2: TLabel
        Left = 35
        Top = 178
        Width = 75
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Password'
      end
      object Label2: TLabel
        Left = 63
        Top = 271
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
        Top = 224
        Width = 49
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Server'
      end
      object lblUser2: TLabel
        Left = 60
        Top = 135
        Width = 35
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'User'
      end
      object lblDatabase2: TLabel
        Left = 35
        Top = 24
        Width = 89
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Database 2'
      end
      object lblDbType2: TLabel
        Left = 73
        Top = 89
        Width = 37
        Height = 25
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Type'
      end
      object sePort2: TSpinEdit
        Left = 120
        Top = 268
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
        Top = 175
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 1
        Text = 'pass2'
      end
      object edtServer2: TEdit
        Left = 120
        Top = 221
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 2
        Text = 'edtServer2'
      end
      object edtUser2: TEdit
        Left = 120
        Top = 132
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 3
        Text = 'User 2'
      end
      object cmbDbType2: TComboBox
        Left = 120
        Top = 89
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = 'MySQL'
        Items.Strings = (
          'MySQL'
          'SQL Server'
          'PostgreSQL'
          'Firebird')
      end
      object chkActive2: TCheckBox
        Left = 120
        Top = 53
        Width = 146
        Height = 26
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Active'
        TabOrder = 5
      end
    end
    object grpbxGeneral: TGroupBox
      Left = 24
      Top = 414
      Width = 853
      Height = 158
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'General'
      TabOrder = 2
      object memoDescr: TMemo
        Left = 22
        Top = 48
        Width = 229
        Height = 85
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Lines.Strings = (
          'Description')
        TabOrder = 0
      end
      object rdgrpUseOption: TRadioGroup
        Left = 575
        Top = 24
        Width = 230
        Height = 121
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'rdgrpUseOption'
        Items.Strings = (
          'Left'
          'Right'
          'Both')
        TabOrder = 1
      end
      object chkDefDescr: TCheckBox
        Left = 261
        Top = 48
        Width = 196
        Height = 26
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Default description'
        TabOrder = 2
      end
      object medtCustom: TMaskEdit
        Left = 275
        Top = 96
        Width = 182
        Height = 33
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        TabOrder = 3
        Text = 'medtCustom'
      end
    end
  end
end
