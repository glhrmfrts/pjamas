{
  MIT License

  Copyright (c) 2024 Guilherme Freitas Nemeth

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

unit JSONPrettyPrint;

{$mode objfpc}{$H+}

interface


uses fpjson;


function ToPrettyJSON(val: TJSONData): String;


implementation


uses Classes, SysUtils, StrUtils;


{ ToPrettyJSON }

function ToPrettyJSON(val: TJSONData): String;
var
  IndentLevel: integer;
  Stream: TMemoryStream;
  Bytes: array of byte;

  procedure WriteString(s: string);
  var
    Bytes: TBytes;
  begin
    Bytes := TEncoding.UTF8.GetBytes(s);
    Stream.WriteBuffer(Bytes[0], Length(Bytes));
  end;

  procedure DoIndent;
  var
    I: integer;
  begin
    for I := 0 to (IndentLevel*2) - 1 do
    begin
      WriteString(' ');
    end;
  end;

  procedure WriteValue(val: TJSONData);
  var
    I: integer;
  begin
    case val.JSONType of
      jtNull, jtBoolean, jtNumber, jtString : WriteString(val.AsJSON);
      jtArray : begin
        WriteString('[' + #10);
        Inc(IndentLevel);
        for I := 0 to val.Count - 1 do
        begin
          DoIndent;
          WriteValue(val.Items[I]);
          if I < val.Count - 1 then WriteString(',' + #10);
        end;
        WriteString(#10);
        Dec(IndentLevel);
        DoIndent;
        WriteString(']');
      end;
      jtObject : begin
        WriteString('{' + #10);
        Inc(IndentLevel);
        for I := 0 to val.Count - 1 do
        begin
          DoIndent;
          WriteString('"' + TJSONObject(val).Names[I] + '": ');
          WriteValue(val.Items[I]);
          if I < val.Count - 1 then WriteString(',' + #10);
        end;
        WriteString(#10);
        Dec(IndentLevel);
        DoIndent;
        WriteString('}');
      end;
    end;
  end;

begin

  try
    Stream := TMemoryStream.Create;

    WriteValue(val);

    Stream.Position := 0;
    SetLength(Bytes, Stream.Size);
    Stream.Read(Bytes[0], Stream.Size);
    Result := TEncoding.UTF8.GetString(Bytes);
  finally
    Stream.Free;
  end;

end;


end.