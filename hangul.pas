unit hangul;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

  // 한글 처리 유닛
type
  TBul = record
    cho: uint8;
    middle: uint8;
    jong: uint8;
  end;

  TJaso = record
    cho: uint8;
    middle: uint8;
    jong: uint8;
  end;

  TUTF16Array = array of word;

const
  NUM_OF_JONG: integer = 28;
  NUM_OF_MID: integer = 21;

function buildJaso(code: word): TJaso;
procedure debugJaso(jaso: TJaso);

function buildBul(jaso: TJaso): TBul;
procedure debugBul(bul: TBul);

function utf8_to_ucs2(src: string): TUTF16Array;

implementation

procedure debugJaso(jaso: TJaso);
begin

  WriteLn('cho = ', jaso.cho);
  WriteLn('middle = ', jaso.middle);
  WriteLn('jong = ', jaso.jong);
end;

(*
utf8 문자열이나 ucs2 문자열중 한글을 자소로 분리하는 라이브러리

지원하는 한글 자소는 아래와 같다. 괄호로 묶인 것은 복자음.
- 초성 ㄱㄲㄴㄷㄸㄹㅁㅂㅃㅅㅆㅇㅈㅉㅊㅋㅌㅍㅎ
- 중성 ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ
- 종성 ㄱㄲ(ㄱㅅ)ㄴ(ㄴㅈ)(ㄴㅎ)ㄷㄹ(ㄹㄱ)(ㄹㅁ)(ㄹㅂ)(ㄹㅅ)(ㄹㅌ)(ㄹㅍ)(ㄹㅎ)ㅁㅂ(ㅂㅅ)ㅅㅆㅇㅈㅊㅋㅌㅍㅎ
*)

function buildJaso(code: word): TJaso;
var
  hancode: word;
  cho: uint8;
  middle: uint8;
  jong: uint8;
begin
  with Result do
  begin
    cho := 0;
    middle := 0;
    jong := 0;
  end;

  if (code and $8000) = $8000 then
  begin
    hancode := code - $ac00;
    jong := hancode mod NUM_OF_JONG;
    middle := uint8(integer((hancode - jong) div NUM_OF_JONG) mod NUM_OF_MID + 1);
    cho := uint8(integer((hancode - jong) div NUM_OF_JONG) div NUM_OF_MID + 1);

    Result.cho := cho;
    Result.middle := middle;
    Result.jong := jong;
  end;
end;

procedure debugBul(bul: TBul);
begin
  WriteLn('cho = ', bul.cho);
  WriteLn('middle = ', bul.middle);
  WriteLn('jong = ', bul.jong);

end;

(*

/// 8x4x4 폰트 세트에서 초성,중성,종성의 벌을 가져오기
///
///    초성
///    초성 1벌: 받침없는 'ㅏㅐㅑㅒㅓㅔㅕㅖㅣ' 와 결합
///    초성 2벌: 받침없는 'ㅗㅛㅡ'
///    초성 3벌: 받침없는 'ㅜㅠ'
///    초성 4벌: 받침없는 'ㅘㅙㅚㅢ'
///    초성 5벌: 받침없는 'ㅝㅞㅟ'
///    초성 6벌: 받침있는 'ㅏㅐㅑㅒㅓㅔㅕㅖㅣ' 와 결합
///    초성 7벌: 받침있는 'ㅗㅛㅜㅠㅡ'
///    초성 8벌: 받침있는 'ㅘㅙㅚㅢㅝㅞㅟ'
///
///    중성
///    중성 1벌: 받침없는 'ㄱㅋ' 와 결합
///    중성 2벌: 받침없는 'ㄱㅋ' 이외의 자음
///    중성 3벌: 받침있는 'ㄱㅋ' 와 결합
///    중성 4벌: 받침있는 'ㄱㅋ' 이외의 자음
///
///    종성
///    종성 1벌: 중성 'ㅏㅑㅘ' 와 결합
///    종성 2벌: 중성 'ㅓㅕㅚㅝㅟㅢㅣ'
///    종성 3벌: 중성 'ㅐㅒㅔㅖㅙㅞ'
///    종성 4벌: 중성 'ㅗㅛㅜㅠㅡ'
*)

function buildBul(jaso: TJaso): TBul;
var
  cho: uint8;
  middle: uint8;
  jong: uint8;
begin

  cho := 0;
  middle := 0;
  jong := 0;

  if jaso.jong = 0 then
  begin
    // 받침이 없는 경우
    if ((jaso.middle >= 1) and (jaso.middle <= 8)) or (jaso.middle = 21) then
    begin
      // ㅏㅐㅑㅒㅓㅔㅕㅖㅣ
      cho := 1;
    end
    else if (jaso.middle = 9) or (jaso.middle = 13) or (jaso.middle = 19) then
    begin
      // ㅗㅛㅡ
      cho := 2;
    end
    else if (jaso.middle = 14) or (jaso.middle = 18) then
    begin
      // ㅜㅠ
      cho := 3;
    end
    else if ((jaso.middle >= 10) and (jaso.middle <= 12)) or (jaso.middle = 20) then
    begin
      // ㅘㅙㅚㅢ
      cho := 4;
    end
    else if (jaso.middle >= 15) and (jaso.middle <= 17) then
    begin
      // ㅝㅞㅟ
      cho := 5;
    end;

    if (jaso.cho >= 1) and (jaso.cho <= 2) then
    begin
      //  ㄱㄲ
      middle := 1;
    end
    else if (jaso.cho >= 3) and (jaso.cho <= 19) then
    begin
      // ㄱㄲ 이외
      middle := 2;
    end
    else
      middle := 0;

    jong := 0;
  end
  else
  begin
    // 받침이 있는 경우
    if ((jaso.middle >= 1) and (jaso.middle <= 8)) or (jaso.middle = 21) then
    begin
      // ㅏㅐㅑㅒㅓㅔㅕㅖㅣ
      cho := 6;
    end
    else if (jaso.middle = 9) or (jaso.middle = 13) or (jaso.middle = 14) or
      (jaso.middle = 18) or (jaso.middle = 19) then
    begin
      // ㅗㅛㅜㅠㅡ
      cho := 7;
    end
    else if ((jaso.middle >= 10) and (jaso.middle <= 12)) or
      ((jaso.middle >= 15) and (jaso.middle <= 17)) or (jaso.middle = 20) then
    begin
      // ㅘㅙㅚㅢㅝㅞㅟ
      cho := 8;
    end
    else
      cho := 0;

    if (jaso.cho >= 1) and (jaso.cho <= 2) then
    begin
      // ㄱㄲ
      middle := 3;
    end
    else if (jaso.cho >= 3) and (jaso.cho <= 19) then
    begin
      // ㄱㄲ 이외
      middle := 4;
    end
    else
      middle := 0;

    if (jaso.middle = 1) or (jaso.middle = 3) or (jaso.middle = 10) then
    begin
      // ㅏㅑㅘ
      jong := 1;
    end
    else if (jaso.middle = 5) or (jaso.middle = 7) or (jaso.middle = 12) or
      (jaso.middle = 15) or (jaso.middle = 17) or (jaso.middle = 20) or
      (jaso.middle = 21) then
    begin
      // ㅓㅕㅚㅝㅟㅢㅣ
      jong := 2;

    end
    else if (jaso.middle = 2) or (jaso.middle = 4) or (jaso.middle = 6) or
      (jaso.middle = 8) or (jaso.middle = 11) or (jaso.middle = 16) then
    begin
      // ㅐㅒㅔㅖㅙㅞ
      jong := 3;
    end
    else if (jaso.middle = 9) or (jaso.middle = 13) or (jaso.middle = 14) or
      (jaso.middle = 18) or (jaso.middle = 19) then
    begin

      // ㅗㅛㅜㅠㅡ
      jong := 4;
    end
    else
      jong := 0;
  end;

  Result.cho := cho;
  Result.middle := middle;
  Result.jong := jong;

end;

function utf8_to_ucs2(src: string): TUTF16Array;
var
  I: integer;
  Len: integer;
  CodePoint: integer;
  C: byte;
begin
  Len := Length(src);
  I := 1;
  SetLength(Result, 0);

  while I <= Len do
  begin
    C := byte(Src[I]);
    if C and $80 = 0 then
    begin
      // 1-byte sequence
      CodePoint := word(C);
      Inc(I);
    end
    else if C and $E0 = $C0 then
    begin
      // 2-byte sequence
      if I + 1 > Len then
        // Invalid
        CodePoint := 0
      else
        CodePoint := ((C and $1f) shl 6) or (byte(src[I + 1]) and $3f);
      Inc(I, 2);
    end
    else if c and $f0 = $e0 then
    begin
      if i + 2 > Len then
        CodePoint := 0
      else
        CodePoint := ((C and $0f) shl 12) or ((byte(src[I + 1]) and $3f) shl 6) or
          (byte(src[I + 2]) and $3f);
      Inc(I, 3);
    end
    else if C and $f8 = $f0 then
    begin
      // 4 Byte
      if I + 3 > Len then
        CodePoint := 0
      else
        CodePoint := ((c and $07) shl 18) or ((byte(src[I + 1]) and $3f) shl 12) or
          ((byte(src[I + 2]) and $3f) shl 6) or (byte(src[I + 3]) and $3f);
      Inc(I, 4);
    end
    else
      CodePoint := 0;

    if CodePoint <= $FFFF then
    begin
      // BMP character (1 UTF-16 unit)
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := CodePoint;
    end
    else
    begin
      // Non-BMP character (surrogate pair)
      CodePoint := CodePoint - $10000;
      SetLength(Result, Length(Result) + 2);
      Result[High(Result) - 1] := $D800 or (CodePoint shr 10);       // High surrogate
      Result[High(Result)] := $DC00 or (CodePoint and $3FF);        // Low surrogate
    end;
  end;
end;

end.
