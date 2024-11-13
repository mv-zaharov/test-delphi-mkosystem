unit uTypeModule;

interface
uses
  System.Generics.Collections,
  System.Classes;

type
  TFileSearchResult = record
    FileCount: Integer;
    FilePaths: TStrings;
  end;

  TTextSearchResult = record
    TextCount: Integer;
    TextPositions: TDictionary<string, TList<Integer>>;
  end;

  PPAnsiCharArray = ^TPAnsiCharArray;
  TPAnsiCharArray = array of PAnsiChar;  // ��������� �������� ������ �� ��������
  //������ ������ � ������� ����� - ��� ���������� ��� ������ ��� ������ ��� �����

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array of Integer;  // �������� ������ � ������� �����

  TProgressCallback = procedure(ID, Progress: Integer; Status: PAnsiChar); stdcall;

implementation

end.
