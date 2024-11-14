object Form1: TForm1
  Left = 219
  Top = 0
  Caption = 'Form1'
  ClientHeight = 661
  ClientWidth = 701
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 642
    Width = 701
    Height = 19
    Panels = <
      item
        Text = #1047#1072#1076#1072#1095#1072': '
        Width = 100
      end
      item
        Text = #1057#1090#1072#1090#1091#1089': '
        Width = 500
      end>
    ExplicitLeft = 176
    ExplicitTop = 248
    ExplicitWidth = 0
  end
  object PanelChooseTask: TPanel
    Left = 0
    Top = 0
    Width = 701
    Height = 394
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 593
    object RadioGroupDLL: TRadioGroup
      Left = 1
      Top = 1
      Width = 699
      Height = 64
      Align = alTop
      Caption = #1044#1086#1089#1090#1091#1087#1085#1099#1077' '#1073#1080#1073#1083#1080#1086#1090#1077#1082#1080':'
      ItemIndex = 0
      Items.Strings = (
        'lib1.dll'
        'lib2.dll')
      TabOrder = 0
      OnClick = RadioGroupDLLClick
      ExplicitWidth = 591
    end
    object Panel1: TPanel
      Left = 1
      Top = 65
      Width = 699
      Height = 72
      Align = alTop
      TabOrder = 1
      ExplicitLeft = 2
      ExplicitTop = 60
      ExplicitWidth = 601
      object Label1: TLabel
        Left = 1
        Top = 1
        Width = 697
        Height = 13
        Align = alTop
        Caption = #1042#1099#1073#1086#1088' '#1079#1072#1076#1072#1095#1080':'
        ExplicitLeft = 0
      end
      object ComboBoxTasks: TComboBox
        Left = 1
        Top = 20
        Width = 344
        Height = 21
        ItemIndex = 0
        TabOrder = 0
        Text = #1055#1086#1080#1089#1082' '#1092#1072#1081#1083#1086#1074' (FindFiles)'
        OnChange = ComboBoxTasksChange
        Items.Strings = (
          #1055#1086#1080#1089#1082' '#1092#1072#1081#1083#1086#1074' (FindFiles)'
          #1055#1086#1080#1089#1082' '#1074#1093#1086#1078#1076#1077#1085#1080#1081' (FindText)')
      end
      object Start: TButton
        Left = 482
        Top = 20
        Width = 75
        Height = 25
        Caption = 'Start'
        TabOrder = 1
        OnClick = StartClick
      end
      object Button1: TButton
        Left = 592
        Top = 20
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 2
        OnClick = Button1Click
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 137
      Width = 699
      Height = 256
      Align = alClient
      TabOrder = 2
      ExplicitLeft = 0
      ExplicitTop = 133
      ExplicitWidth = 632
      object Label2: TLabel
        Left = 1
        Top = 1
        Width = 697
        Height = 20
        Align = alTop
        Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099' '#1076#1083#1103' '#1079#1072#1087#1091#1089#1082#1072' '#1079#1072#1076#1072#1095#1080':'
        ExplicitTop = 6
        ExplicitWidth = 630
      end
      object LabeledEditPath: TLabeledEdit
        Left = 7
        Top = 35
        Width = 550
        Height = 21
        EditLabel.Width = 174
        EditLabel.Height = 13
        EditLabel.Caption = #1042#1074#1077#1076#1080#1090#1077' '#1076#1080#1088#1077#1082#1090#1086#1088#1080#1102' '#1076#1083#1103' '#1087#1086#1080#1089#1082#1072':'
        TabOrder = 0
      end
      object Memo2: TMemo
        Left = 7
        Top = 160
        Width = 550
        Height = 51
        Lines.Strings = (
          'Memo1')
        TabOrder = 1
      end
      object LabeledEditArcSrc: TLabeledEdit
        Left = 7
        Top = 80
        Width = 550
        Height = 21
        EditLabel.Width = 113
        EditLabel.Height = 13
        EditLabel.Caption = #1048#1089#1090#1086#1095#1085#1080#1082' '#1076#1083#1103' '#1072#1088#1093#1080#1074#1072':'
        TabOrder = 2
      end
      object LabeledEditArcDst: TLabeledEdit
        Left = 7
        Top = 125
        Width = 550
        Height = 21
        EditLabel.Width = 185
        EditLabel.Height = 13
        EditLabel.Caption = #1044#1080#1088#1077#1082#1090#1086#1088#1080#1103' '#1080' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1072#1088#1093#1080#1074#1072
        TabOrder = 3
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 394
    Width = 701
    Height = 215
    Align = alTop
    TabOrder = 2
    Visible = False
    ExplicitTop = 399
    object Label3: TLabel
      Left = 1
      Top = 1
      Width = 699
      Height = 25
      Align = alTop
      Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090#1099' '#1074#1099#1087#1086#1083#1085#1077#1085#1080#1103' '#1079#1072#1076#1072#1095':'
      ExplicitLeft = 2
    end
    object StringGridTask: TStringGrid
      Left = 1
      Top = 26
      Width = 360
      Height = 188
      Align = alLeft
      ColCount = 3
      FixedColor = clActiveBorder
      RowCount = 3
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect, goFixedColClick, goFixedRowDefAlign]
      TabOrder = 0
      OnSelectCell = StringGridTaskSelectCell
      ExplicitLeft = 2
      ExplicitTop = 32
      RowHeights = (
        24
        24
        24)
    end
    object Memo1: TMemo
      Left = 380
      Top = 32
      Width = 300
      Height = 169
      Lines.Strings = (
        'Memo1')
      TabOrder = 1
    end
  end
end
