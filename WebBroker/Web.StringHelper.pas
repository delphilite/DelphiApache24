{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Web.StringHelper;

interface

uses
  System.SysUtils, Generics.Collections;

type
  TStringSplitOptions = (
    None, ExcludeEmpty
  );

  function StringChars(const S: string; I: Integer): Char;
  function StringContains(const S: string; const Value: string): Boolean;

  function StringIndexOf(const S: string; const Value: string): Integer;

  function StringIndexOfAny(const S: string; const AnyOf: array of Char; StartIndex, Count: Integer): Integer; overload;
  function StringIndexOfAny(const S: string; const Values: array of string; var Index: Integer): Integer; overload;

  function StringIsEmpty(const S: string): Boolean;
  function StringJoin(const Separator: string; const Values: TArray<string>): string;
  function StringPadLeft(const S: string; TotalWidth: Integer; PaddingChar: Char): string;

  function StringLow(const S: string): Integer;
  function StringHigh(const S: string): Integer;

  function StringRemove(const S: string; StartIndex, Count: Integer): string; overload;
  function StringRemove(const S: string; StartIndex: Integer): string; overload;

  function StringSplit(const S: string; const Separator: array of Char): TArray<string>; overload;
  function StringSplit(const S: string; const Separator: array of Char; Count: Integer): TArray<string>; overload;
  function StringSplit(const S: string; const Separator: array of Char; Count: Integer; Options: TStringSplitOptions): TArray<string>; overload;
  function StringSplit(const S: string; const Separator: array of string): TArray<string>; overload;
  function StringSplit(const S: string; const Separator: array of string; Count: Integer; Options: TStringSplitOptions): TArray<string>; overload;

  function StringStartsWith(const S: string; const Value: string): Boolean; overload;
  function StringStartsWith(const S: string; const Value: string; IgnoreCase: Boolean): Boolean; overload;

  function StringEndsWith(const S: string; const Value: string): Boolean; overload;
  function StringEndsWith(const S: string; const Value: string; IgnoreCase: Boolean): Boolean; overload;

  function StringInsert(var S: string; StartIndex: Integer; const Value: string): string;

  function StringSubstring(const S: string; I: Integer): string; overload;
  function StringSubstring(const S: string; StartIndex, Length: Integer): string; overload;
  function StringSubstringToInteger(const S: string; I: Integer): Integer;

  function StringTrimRight(const S: string; const TrimChars: array of Char): string;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF MSWINDOWS}
  System.SysConst, System.StrUtils;

const
  Empty = '';

function StringChars(const S: string; I: Integer): Char;
begin
  if (I < 0) or (I >= Length(S)) then
    raise ERangeError.CreateResFmt(@SCharIndexOutOfBounds, [I]);
  Result := S[I + 1];
end;

function StringContains(const S: string; const Value: string): Boolean;
begin
  Result := System.Pos(Value, S) > 0;
end;

function StringIndexOf(const S: string; const Value: string): Integer; overload;
begin
  Result := System.Pos(Value, S) - 1;
end;

function StringIndexOfAny(const S: string; const AnyOf: array of Char; StartIndex, Count: Integer): Integer;
var
  I: Integer;
  C: Char;
  Max: Integer;
begin
  if (StartIndex + Count) >= Length(S) then
    Max := Length(S)
  else
    Max := StartIndex + Count;

  I := StartIndex;
  while I < Max do
  begin
    for C in AnyOf do
      if S[I] = C then
        Exit(I);
    Inc(I);
  end;
  Result := -1;
end;

function StringIndexOfAny(const S: string; const Values: array of string; var Index: Integer): Integer;
var
  C, P: Integer;
begin
  for C := 0 to High(Values) do
  begin
    P := StringIndexOf(S, Values[C]);
    if P >= 0 then
    begin
      Index := C;
      Exit(P);
    end;
  end;
  Result := -1;
end;

function StringIsEmpty(const S: string): Boolean;
begin
  Result := S = '';
end;

function StringJoin(const Separator: string; const Values: TArray<string>): string;
var
  I, Len: Integer;
begin
  Len := System.Length(Values);
  if Len <= 0 then
    Result := ''
  else begin
    Result := Values[0];
    for I := 1 to Len - 1 do
      Result := Result + Separator + Values[I];
  end;
end;

function StringPadLeft(const S: string; TotalWidth: Integer; PaddingChar: Char): string;
begin
  TotalWidth := TotalWidth - Length(S);
  if TotalWidth > 0 then
    Result := System.StringOfChar(PaddingChar, TotalWidth) + S
  else
    Result := S;
end;

function StringLow(const S: string): Integer;
begin
{$IFDEF NEXTGEN}
  Result := 0;
{$ELSE}
  Result := 1;
{$ENDIF}
end;

function StringHigh(const S: string): Integer;
begin
{$IFDEF NEXTGEN}
  Result := Length(S) - 1;
{$ELSE}
  Result := Length(S);
{$ENDIF}
end;

function StringRemove(const S: string; StartIndex, Count: Integer): string;
begin
  Result := S;
  System.Delete(Result, StartIndex + 1, Count);
end;

function StringRemove(const S: string; StartIndex: Integer): string;
begin
  Result := S;
  System.Delete(Result, StartIndex + 1, Length(S));
end;

function StringSplit(const S: string; const Separator: array of Char): TArray<string>;
begin
  Result := StringSplit(S, Separator, MaxInt, None);
end;

function StringSplit(const S: string; const Separator: array of Char; Count: Integer): TArray<string>;
begin
  Result := StringSplit(S, Separator, Count, None);
end;

function StringSplit(const S: string; const Separator: array of Char; Count: Integer;
  Options: TStringSplitOptions): TArray<string>;
var
  P: Integer;
  Total: Integer;
  D: string;
  ToSplit: string;
begin
  SetLength(Result, 0);
  Total := 0;
  ToSplit := S;
  P := StringIndexOfAny(ToSplit, Separator, 0, Length(ToSplit));
  while (P >= 0) and (Total < Count) do
  begin
    D := StringSubstring(ToSplit, 0, P);
    if (D <> Empty) or ((D = Empty) and (Options <> ExcludeEmpty)) then
    begin
      Inc(Total);
      SetLength(Result, Total);
      Result[Total - 1] := D;
    end;
    ToSplit := StringSubstring(ToSplit, P + 1);
    P := StringIndexOfAny(ToSplit, Separator, 0, Length(ToSplit));
  end;

  if (ToSplit <> Empty) and (Total < Count) then
  begin
    Inc(Total);
    SetLength(Result, Total);
    Result[Total - 1] := ToSplit;
  end;
end;

function StringSplit(const S: string; const Separator: array of string): TArray<string>;
begin
  Result := StringSplit(S, Separator, MaxInt, None);
end;

function StringSplit(const S: string; const Separator: array of string; Count: Integer; Options: TStringSplitOptions): TArray<string>;
var
  P: Integer;
  Total: Integer;
  Index: Integer;
  D, ToSplit: string;
begin
  SetLength(Result, 0);
  Total := 0;
  ToSplit := S;
  P := StringIndexOfAny(ToSplit, Separator, Index);
  while (P >= 0) and (Total < Count) do
  begin
    D := StringSubstring(ToSplit, 0, P);
    if (D <> Empty) or ((D = Empty) and (Options <> ExcludeEmpty)) then
    begin
      Inc(Total);
      SetLength(Result, Total);
      Result[Total - 1] := D;
    end;
    ToSplit := StringSubstring(ToSplit, P + Length(Separator[Index]));
    P := StringIndexOfAny(ToSplit, Separator, Index);
  end;

  if (ToSplit <> Empty) and (Total < Count) then
  begin
    Inc(Total);
    SetLength(Result, Total);
    Result[Total - 1] := ToSplit;
  end;
end;

function StringStartsWith(const S: string; const Value: string): Boolean;
begin
  Result := StringStartsWith(S, Value, False);
end;

function StringStartsWith(const S: string; const Value: string; IgnoreCase: Boolean): Boolean;
begin
  if not IgnoreCase then
    Result := System.SysUtils.StrLComp(PChar(S), PChar(Value), Length(S)) = 0
  else
    Result := System.SysUtils.StrLIComp(PChar(S), PChar(Value), Length(S)) = 0;
end;

function StringEndsWith(const S: string; const Value: string): Boolean; overload;
begin
  Result := StringEndsWith(S, Value, False);
end;

function StringEndsWith(const S: string; const Value: string; IgnoreCase: Boolean): Boolean;

  function EndsText(const ASubText, AText: string): Boolean;
  var
    SubTextLocation: Integer;
  begin
    if ASubText = Empty then
      Result := True
    else
    begin
      SubTextLocation := Length(AText) - Length(ASubText);
      if (SubTextLocation >= 0) and (ByteType(AText, SubTextLocation) <> mbTrailByte) then
        Result := AnsiStrIComp(PChar(ASubText), PChar(@AText[SubTextLocation])) = 0
      else
        Result := False;
    end;
  end;
var
  SubTextLocation: Integer;
begin
  if IgnoreCase then
    Result := EndsText(Value, S)
  else
  if Value = Empty then
    Result := True
  else
  begin
    SubTextLocation := Length(S) - Length(Value);
    if (SubTextLocation >= 0) and (ByteType(S, SubTextLocation) <> mbTrailByte) then
      Result := AnsiStrComp(PChar(Value), PChar(@S[SubTextLocation])) = 0
    else
      Result := False;
  end;
end;

function StringInsert(var S: string; StartIndex: Integer; const Value: string): string;
begin
  System.Insert(Value, S, StartIndex + 1);
  Result := S;
end;

function StringSubstring(const S: string; I: Integer): string;
begin
  Result := System.Copy(S, I + 1, MaxInt);
end;

function StringSubstring(const S: string; StartIndex, Length: Integer): string;
begin
  Result := System.Copy(S, StartIndex + 1, Length);
end;

function StringSubstringToInteger(const S: string; I: Integer): Integer;
var
  D: string;
begin
  D := System.Copy(S, I + 1, MaxInt);
  Result := StrToInt(D);
end;

function StringTrimRight(const S: string; const TrimChars: array of Char): string;

  function CharInArray(const C: Char; const InArray: array of Char): Boolean;
  var
    AChar: Char;
  begin
    for AChar in InArray do
      if AChar = C then Exit(True);
    Result := False;
  end;
var
  I: Integer;
begin
  I := StringHigh(S);
  if (I >= StringLow(S)) and (not CharInArray(S[I], TrimChars)) then
    Exit(S);
  Dec(I);
  while (I >= 0) and (CharInArray(S[I], TrimChars)) do
    Dec(I);
  Result := StringSubString(S, 0, I + 1);
end;

end.
